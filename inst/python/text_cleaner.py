# Taken from: https://github.com/madisonmay/CommonRegex under MIT Copyright License
import re

date             = re.compile('(?:(?<!\:)(?<!\:\d)[0-3]?\d(?:st|nd|rd|th)?\s+(?:of\s+)?(?:jan\.?|january|feb\.?|february|mar\.?|march|apr\.?|april|may|jun\.?|june|jul\.?|july|aug\.?|august|sep\.?|september|oct\.?|october|nov\.?|november|dec\.?|december)|(?:jan\.?|january|feb\.?|february|mar\.?|march|apr\.?|april|may|jun\.?|june|jul\.?|july|aug\.?|august|sep\.?|september|oct\.?|october|nov\.?|november|dec\.?|december)\s+(?<!\:)(?<!\:\d)[0-3]?\d(?:st|nd|rd|th)?)(?:\,)?\s*(?:\d{4})?|[0-3]?\d[-\./][0-3]?\d[-\./]\d{2,4}', re.IGNORECASE)
time             = re.compile(r"""\b((1[0-2]|0?[1-9]):([0-5][0-9])(\s?[AaPp][Mm])?|([01]?[0-9]|2[0-3]):([0-5][0-9]))\b""", re.IGNORECASE)
phone            = re.compile('''((?:(?<![\d-])(?:\+?\d{1,3}[-.\s*]?)?(?:\(?\d{3}\)?[-.\s*]?)?\d{3}[-.\s*]?\d{4}(?![\d-]))|(?:(?<![\d-])(?:(?:\(\+?\d{2}\))|(?:\+?\d{2}))\s*\d{2}\s*\d{3}\s*\d{4}(?![\d-])))''')
phones_with_exts = re.compile('((?:(?:\+?1\s*(?:[.-]\s*)?)?(?:\(\s*(?:[2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\s*\)|(?:[2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\s*(?:[.-]\s*)?)?(?:[2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\s*(?:[.-]\s*)?(?:[0-9]{4})(?:\s*(?:#|x\.?|ext\.?|extension)\s*(?:\d+)?))', re.IGNORECASE)
link             = re.compile('(?i)((?:https?://|www\d{0,3}[.])?[a-z0-9.\-]+[.](?:(?:international)|(?:construction)|(?:contractors)|(?:enterprises)|(?:photography)|(?:immobilien)|(?:management)|(?:technology)|(?:directory)|(?:education)|(?:equipment)|(?:institute)|(?:marketing)|(?:solutions)|(?:builders)|(?:clothing)|(?:computer)|(?:democrat)|(?:diamonds)|(?:graphics)|(?:holdings)|(?:lighting)|(?:plumbing)|(?:training)|(?:ventures)|(?:academy)|(?:careers)|(?:company)|(?:domains)|(?:florist)|(?:gallery)|(?:guitars)|(?:holiday)|(?:kitchen)|(?:recipes)|(?:shiksha)|(?:singles)|(?:support)|(?:systems)|(?:agency)|(?:berlin)|(?:camera)|(?:center)|(?:coffee)|(?:estate)|(?:kaufen)|(?:luxury)|(?:monash)|(?:museum)|(?:photos)|(?:repair)|(?:social)|(?:tattoo)|(?:travel)|(?:viajes)|(?:voyage)|(?:build)|(?:cheap)|(?:codes)|(?:dance)|(?:email)|(?:glass)|(?:house)|(?:ninja)|(?:photo)|(?:shoes)|(?:solar)|(?:today)|(?:aero)|(?:arpa)|(?:asia)|(?:bike)|(?:buzz)|(?:camp)|(?:club)|(?:coop)|(?:farm)|(?:gift)|(?:guru)|(?:info)|(?:jobs)|(?:kiwi)|(?:land)|(?:limo)|(?:link)|(?:menu)|(?:mobi)|(?:moda)|(?:name)|(?:pics)|(?:pink)|(?:post)|(?:rich)|(?:ruhr)|(?:sexy)|(?:tips)|(?:wang)|(?:wien)|(?:zone)|(?:biz)|(?:cab)|(?:cat)|(?:ceo)|(?:com)|(?:edu)|(?:gov)|(?:int)|(?:mil)|(?:net)|(?:onl)|(?:org)|(?:pro)|(?:red)|(?:tel)|(?:uno)|(?:xxx)|(?:ac)|(?:ad)|(?:ae)|(?:af)|(?:ag)|(?:ai)|(?:al)|(?:am)|(?:an)|(?:ao)|(?:aq)|(?:ar)|(?:as)|(?:at)|(?:au)|(?:aw)|(?:ax)|(?:az)|(?:ba)|(?:bb)|(?:bd)|(?:be)|(?:bf)|(?:bg)|(?:bh)|(?:bi)|(?:bj)|(?:bm)|(?:bn)|(?:bo)|(?:br)|(?:bs)|(?:bt)|(?:bv)|(?:bw)|(?:by)|(?:bz)|(?:ca)|(?:cc)|(?:cd)|(?:cf)|(?:cg)|(?:ch)|(?:ci)|(?:ck)|(?:cl)|(?:cm)|(?:cn)|(?:co)|(?:cr)|(?:cu)|(?:cv)|(?:cw)|(?:cx)|(?:cy)|(?:cz)|(?:de)|(?:dj)|(?:dk)|(?:dm)|(?:do)|(?:dz)|(?:ec)|(?:ee)|(?:eg)|(?:er)|(?:es)|(?:et)|(?:eu)|(?:fi)|(?:fj)|(?:fk)|(?:fm)|(?:fo)|(?:fr)|(?:ga)|(?:gb)|(?:gd)|(?:ge)|(?:gf)|(?:gg)|(?:gh)|(?:gi)|(?:gl)|(?:gm)|(?:gn)|(?:gp)|(?:gq)|(?:gr)|(?:gs)|(?:gt)|(?:gu)|(?:gw)|(?:gy)|(?:hk)|(?:hm)|(?:hn)|(?:hr)|(?:ht)|(?:hu)|(?:id)|(?:ie)|(?:il)|(?:im)|(?:in)|(?:io)|(?:iq)|(?:ir)|(?:is)|(?:it)|(?:je)|(?:jm)|(?:jo)|(?:jp)|(?:ke)|(?:kg)|(?:kh)|(?:ki)|(?:km)|(?:kn)|(?:kp)|(?:kr)|(?:kw)|(?:ky)|(?:kz)|(?:la)|(?:lb)|(?:lc)|(?:li)|(?:lk)|(?:lr)|(?:ls)|(?:lt)|(?:lu)|(?:lv)|(?:ly)|(?:ma)|(?:mc)|(?:md)|(?:me)|(?:mg)|(?:mh)|(?:mk)|(?:ml)|(?:mm)|(?:mn)|(?:mo)|(?:mp)|(?:mq)|(?:mr)|(?:ms)|(?:mt)|(?:mu)|(?:mv)|(?:mw)|(?:mx)|(?:my)|(?:mz)|(?:na)|(?:nc)|(?:ne)|(?:nf)|(?:ng)|(?:ni)|(?:nl)|(?:no)|(?:np)|(?:nr)|(?:nu)|(?:nz)|(?:om)|(?:pa)|(?:pe)|(?:pf)|(?:pg)|(?:ph)|(?:pk)|(?:pl)|(?:pm)|(?:pn)|(?:pr)|(?:ps)|(?:pt)|(?:pw)|(?:py)|(?:qa)|(?:re)|(?:ro)|(?:rs)|(?:ru)|(?:rw)|(?:sa)|(?:sb)|(?:sc)|(?:sd)|(?:se)|(?:sg)|(?:sh)|(?:si)|(?:sj)|(?:sk)|(?:sl)|(?:sm)|(?:sn)|(?:so)|(?:sr)|(?:st)|(?:su)|(?:sv)|(?:sx)|(?:sy)|(?:sz)|(?:tc)|(?:td)|(?:tf)|(?:tg)|(?:th)|(?:tj)|(?:tk)|(?:tl)|(?:tm)|(?:tn)|(?:to)|(?:tp)|(?:tr)|(?:tt)|(?:tv)|(?:tw)|(?:tz)|(?:ua)|(?:ug)|(?:uk)|(?:us)|(?:uy)|(?:uz)|(?:va)|(?:vc)|(?:ve)|(?:vg)|(?:vi)|(?:vn)|(?:vu)|(?:wf)|(?:ws)|(?:ye)|(?:yt)|(?:za)|(?:zm)|(?:zw))(?:/[^\s()<>]+[^\s`!()\[\]{};:\'".,<>?\xab\xbb\u201c\u201d\u2018\u2019])?)', re.IGNORECASE)
email            = re.compile("([a-z0-9!#$%&'*+\/=?^_`{|.}~-]+@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?)", re.IGNORECASE)
ip               = re.compile('(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)', re.IGNORECASE)
ipv6             = re.compile('\s*(?!.*::.*::)(?:(?!:)|:(?=:))(?:[0-9a-f]{0,4}(?:(?<=::)|(?<!::):)){6}(?:[0-9a-f]{0,4}(?:(?<=::)|(?<!::):)[0-9a-f]{0,4}(?:(?<=::)|(?<!:)|(?<=:)(?<!::):)|(?:25[0-4]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-4]|2[0-4]\d|1\d\d|[1-9]?\d)){3})\s*', re.VERBOSE|re.IGNORECASE|re.DOTALL)
price            = re.compile('[$]\s?[+-]?[0-9]{1,3}(?:(?:,?[0-9]{3}))*(?:\.[0-9]{1,2})?')
credit_card      = re.compile('((?:(?:\\d{4}[- ]?){3}\\d{4}|\\d{15,16}))(?![\\d])')
btc_address      = re.compile('(?<![a-km-zA-HJ-NP-Z0-9])[13][a-km-zA-HJ-NP-Z0-9]{26,33}(?![a-km-zA-HJ-NP-Z0-9])')
street_address   = re.compile('\d{1,4} [\w\s]{1,20}(?:street|st|avenue|ave|road|rd|highway|hwy|square|sq|trail|trl|drive|dr|court|ct|park|parkway|pkwy|circle|cir|boulevard|blvd)\W?(?=\s|$)', re.IGNORECASE)
zip_code         = re.compile(r'\b\d{5}(?:[-\s]\d{4})?\b')
po_box           = re.compile(r'P\.? ?O\.? Box \d+', re.IGNORECASE)
ssn              = re.compile('(?!000|666|333)0*(?:[0-6][0-9][0-9]|[0-7][0-6][0-9]|[0-7][0-7][0-2])[- ]?(?!00)[0-9]{2}[- ]?(?!0000)[0-9]{4}')
at_symbol_data   = re.compile("([a-z0-9!#$%&'*+\/=?^_`{|.}~-])*([@])+([a-z0-9!#$%&'*+\/=?^_`{|.}~-])*", re.IGNORECASE)
# Removed "ME", "OR", and "OK" since they were used more as words than as state abbreviations.
state_2char      = re.compile('([\W]|^)(?:AL|AK|AS|AZ|AR|CA|CO|CT|DE|DC|FM|FL|GA|GU|HI|ID|IL|IN|IA|KS|KY|LA|MH|MD|MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|MP|OH|PW|PA|PR|RI|SC|SD|TN|TX|UT|VT|VI|VA|WA|WV|WI|WY)(\.?)([\W]|^)')
state_abbr       = re.compile('([\W]|^)(?:Ala|Ariz|Ark|Calif|Colo|Conn|Del|D.C|Fla|Flor|Ga|H.I|Ill|Ind|Kan|Kans|Ken|Kent|Mass|Mich|Mont|Neb|Nebr|Nev|N.H|N.Mex|N.Y|N.C|N. Dak|N.D|Okla|Ore|Oreg|Penn|Penna|R.I|S.C|S.Dak|S.D|Tenn|Tex|Wash|W. Va|W.V|Wis|Wisc|Wyo|P.R|V.I)(\.)([\W]|^)')

regexes = {
  "dates"            : date,
  "times"            : time,
  "phones"           : phone,
  "phones_with_exts" : phones_with_exts,
  "links"            : link,
  "emails"           : email,
  "ips"              : ip,
  "ipv6s"            : ipv6,
  "prices"           : price,
  "credit_cards"     : credit_card,
  "btc_addresses"    : btc_address,
  "street_addresses" : street_address,
  "zip_codes"        : zip_code,
  "po_boxes"         : po_box,
  "ssn_number"       : ssn,
  "at_symbol"        : at_symbol_data,
  "state_2char"      : state_2char,
  "state_abbr"       : state_abbr,
}

def remove_pii(text, 
            replace = True,
            date = True, 
            time = True,
            phone = True,
            email = True,
            ip = True,
            money = True,
            creditcard = True,
            bitcoin = True,
            location = True,
            ssn = True,
            at_symbol = True,
            url = True):
    string_match_func_list = []
    if date: 
        string_match_func_list.append(("<DATE_STRING>", regexes["dates"].finditer))
    if time: string_match_func_list.append(("<TIME>", regexes["times"].finditer))
    if ssn: string_match_func_list.append(("<SSN>", regexes["ssn_number"].finditer))
    if phone: 
        string_match_func_list.append(("<PHONE_NUMBER>", regexes["phones_with_exts"].finditer))
        string_match_func_list.append(("<PHONE_NUMBER>", regexes["phones"].finditer))
    if email: string_match_func_list.append(("<EMAIL_ADDRESS>", regexes["emails"].finditer))
    if ip: 
        string_match_func_list.append(("<IP_ADDRESS>", regexes["ips"].finditer))
        string_match_func_list.append(("<IP_ADDRESS>", regexes["ipv6s"].finditer))
    if money: string_match_func_list.append(("<MONEY>", regexes["prices"].finditer))
    if creditcard: string_match_func_list.append(("<CREDITCARD>", regexes["credit_cards"].finditer))
    if bitcoin: string_match_func_list.append(("<BITCOIN_ADDRESS>", regexes["btc_addresses"].finditer))
    if location: 
        string_match_func_list.append(("<STREET_ADDRESS>", regexes["street_addresses"].finditer))
        string_match_func_list.append(("<ZIP_CODE>", regexes["zip_codes"].finditer))
        string_match_func_list.append(("<PO_BOX>", regexes["po_boxes"].finditer))
        string_match_func_list.append(("<STATE>", regexes["state_2char"].finditer))
        string_match_func_list.append(("<STATE>", regexes["state_abbr"].finditer))
    
    if at_symbol: string_match_func_list.append(("<@_SYMBOL>", regexes["at_symbol"].finditer))
    if url: string_match_func_list.append( ("<URL>", regexes["links"].finditer))
    
    for string_match_func in string_match_func_list:
        for match_item in string_match_func[1](text):
            str_to_remove = text[match_item.start():match_item.end()]
            if replace:
                text = text.replace(str_to_remove, string_match_func[0])
            else:
                text = text.replace(str_to_remove, "")
    return text
