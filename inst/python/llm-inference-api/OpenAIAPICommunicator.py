from openai import OpenAI

class OpenAICommunicator:
    def __init__(self, api_key: str, model_name: str, system_prompt: str = None,
                 max_tokens: int = None, reasoning_parameters: dict = None,
                 base_url: str = None):
        """
        Communicator for OpenAI models through the responses API.

        Args:
            api_key: API key for the endpoint (read on the R side and passed in).
            model_name: model to query, e.g. "gpt-5-mini".
            system_prompt: optional system prompt, sent as `instructions` on every call.
            max_tokens: default cap on output tokens (reasoning tokens included); None leaves it to the API.
            reasoning_parameters: default reasoning config, e.g. {"effort": "low", "summary": "auto"}.
            base_url: optional endpoint URL for OpenAI-compatible servers (Azure, vLLM, ...);
                None uses the default OpenAI endpoint.
        """
        self.client = OpenAI(api_key=api_key, base_url=base_url)
        self.model_name = model_name
        self.system_prompt = system_prompt
        self.max_tokens = max_tokens
        self.reasoning_parameters = reasoning_parameters

    def generate(self, context=None, previous_response_id:str=None,
                 reasoning_parameters:dict=None, max_output_tokens:int=None):
        """
        Send a message or a conversation to the model and return the reply as a dict.

        Args:
            context: a list of message packets, a single packet (dict), or a plain string
                for a single user message. Each packet is {"role": ..., "content": ...} with
                role one of "system", "user", "assistant".

                Single turn:
                    [{"role": "system", "content": "You are a helpful assistant."},
                     {"role": "user", "content": "Summarize this text in one sentence: ..."}]

                Multi turn (earlier turns passed back in full):
                    [{"role": "system", "content": "You are a helpful assistant."},
                     {"role": "user", "content": "What is the capital of France?"},
                     {"role": "assistant", "content": "Paris."},
                     {"role": "user", "content": "And its population?"}]

            previous_response_id: optional `response_id` of an earlier call; the server then
                prepends that conversation, so `context` only needs the new user message(s).
                Supported by OpenAI's responses API only.
            reasoning_parameters: overrides the default reasoning config for this call.
            max_output_tokens: overrides the default output-token cap for this call.

        Returns:
            dict with keys:
                text: the model's reply.
                reasoning: reasoning summary text ("" if none was returned).
                response_id: id to pass as previous_response_id on a later call.
                model: model that served the request.
                status: "completed", or "incomplete" (e.g. the token cap was hit).
                usage: token counts as a dict, or None.
        """
        if isinstance(context, dict):
            context = [context]

        # override self.max_tokens if max_output_tokens is present
        max_tokens = max_output_tokens if max_output_tokens is not None else self.max_tokens
        reasoning_params = reasoning_parameters if reasoning_parameters is not None else self.reasoning_parameters

        # system prompt goes in as instructions; it is not carried over by previous_response_id, so send it every call
        # just present the previous conversation id if previous_response_id is Not NULL, else combine it with context
        if previous_response_id is not None:
            response = self.client.responses.create(
                model = self.model_name,
                instructions = self.system_prompt,
                max_output_tokens = max_tokens,
                input = context,
                previous_response_id = previous_response_id,
                reasoning = reasoning_params
            )
        else:
            response = self.client.responses.create(
                model = self.model_name,
                instructions = self.system_prompt,
                max_output_tokens = max_tokens,
                input = context,
                reasoning = reasoning_params
            )

        return self.parse_response(response)

    @staticmethod
    def parse_response(response):
        """
        Flatten a responses API Response object into a plain dict that reticulate can hand to R.

        Args:
            response: the Response object returned by client.responses.create.

        Returns:
            dict with keys text, reasoning, response_id, model, status, usage (see `generate`).
        """
        reasoning = []
        for item in (response.output or []):
            if item.type == "reasoning":
                for s in (item.summary or []):
                    reasoning.append(s.text)

        return {
            "text": response.output_text,
            "reasoning": "\n".join(reasoning),
            "response_id": response.id,
            "model": response.model,
            "status": response.status,
            "usage": response.usage.model_dump() if response.usage is not None else None,
        }
