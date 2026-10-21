import argparse
import json
import os

from OpenAIAPICommunicator import OpenAICommunicator

def get_api_key(api_key_file, api_key_name='default'):

    if os.path.exists(api_key_file):
        keys = json.load(open(api_key_file))
        if api_key_name in keys:
            return keys[api_key_name]
        else:
            raise ValueError("API Key name {} not found in the api key file".format(api_key_name))
    else:
        raise ValueError("API Key file not found at {}".format(api_key_file))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Single-turn test for OpenAICommunicator")
    parser.add_argument("--api_key_file", required=True, help="path to the JSON api key file")
    parser.add_argument("--api_key_name", default="default", help="key name inside the api key file")
    parser.add_argument("--model_name", default="gpt-5-mini")
    parser.add_argument("--base_url", default=None)
    args = parser.parse_args()

    communicator = OpenAICommunicator(
        api_key = get_api_key(args.api_key_file, args.api_key_name),
        model_name = args.model_name,
        base_url = args.base_url
    )

    context = [
        {"role": "system", "content": "You are a helpful assistant. Answer in one short sentence."},
        {"role": "user", "content": "What is the capital of France?"}
    ]

    output = communicator.generate(context)

    assert isinstance(output, dict)
    assert output["text"], "empty response text (status: {})".format(output["status"])
    print(json.dumps(output, indent=2))
