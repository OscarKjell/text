# -*- coding: utf-8 -*-
import os, glob, json, re
from collections import OrderedDict
from transformers import TRANSFORMERS_CACHE
from tokenizers import Tokenizer
from transformers import AutoTokenizer, AutoModelForMaskedLM  # for test only

modelRegex = "huggingface\.co\/(.*)(pytorch_model\.bin$|resolve\/main\/tf_model\.h5$)"

def nameFinder(name_):
    
    if name_.find("/resolve/main") != -1:
        return name_.split("/resolve/main")[0]
    else:
        return name_

def writeNamesTransformers(fileJ, cachedModels, cachedTokenizers):
    
    with open(fileJ) as j:
        data = json.load(j)
        isM = re.search(modelRegex, data['url'])
        if isM:
            temp = isM.group(1)[:-1]
            temp = nameFinder(temp)
            cachedModels[temp] = fileJ
        else:
            temp = data['url'].partition('huggingface.co/')[2]
            temp = nameFinder(temp)
            cachedTokenizers[temp] = fileJ

    return (cachedModels, cachedTokenizers)
    

def textModelsPy():
    
    metaFiles = glob.glob(TRANSFORMERS_CACHE + '/models--*')
    print(f"metaFiles: \n'): {metaFiles}")

    
    cachedModels = {}
    cachedTokenizers = {}
                
    for file in metaFiles:
        cachedModels, cachedTokenizers = writeNamesTransformers(
                                             file
                                             ,cachedModels
                                             ,cachedTokenizers
                                             )   
    
    if cachedTokenizers:
        cachedTokenizers = OrderedDict(sorted(cachedTokenizers.items(), key=lambda k: k[0]))

    if cachedModels and not cachedTokenizers:
        returnTarget = (list(cachedModels.keys()), ("NoTokenizersAvailable"))
        return tuple(returnTarget)
    elif not cachedModels and cachedTokenizers:
        returnTarget = (("NoModelsAvailable"), list(cachedTokenizers.keys()))
        return tuple(returnTarget)
    elif not cachedModels and not cachedTokenizers:
        returnTarget = (("NoModelsAvailable"),("NoTokenizersAvailable"))
        return tuple(returnTarget)
    else:
        returnTarget = (list(cachedModels.keys()), list(cachedTokenizers.keys()))
        return tuple(returnTarget)

def textModelsRMPy(target="default"):

    # transformerLists = textModelsPy()
    metaFiles = glob.glob(TRANSFORMERS_CACHE + '/*.json')

    toDelete = 0
    targetFind = 0
    for file in metaFiles:
        with open(file) as j:
            data = json.load(j)
            if isinstance(target, str):
                if data['url'].find(target) != -1:
                    toDelete = file.find(".json")
                    targetFind += 1
        if toDelete != 0:
            if os.path.exists(file[0:toDelete]): os.remove(file[0:toDelete])
            if os.path.exists(file[0:toDelete] + ".json"): os.remove(file[0:toDelete] + ".json")
            if os.path.exists(file[0:toDelete] + ".lock"): os.remove(file[0:toDelete] + ".lock")
            toDelete = 0
    if targetFind == 0:
        print("Model(s) " + target + " do/does not exist!")

    return 0

# main for test
if __name__ == '__main__':

     tokenizer = AutoTokenizer.from_pretrained("bert-base-uncased")
     model = AutoModelForMaskedLM.from_pretrained("bert-base-uncased")

     # Show the downloaded model.
     #temp = textModelsPy()
     tokenizer1()
     #for a in temp:
     #    print(a)

#     textModelsRMPy(target="bert-base-uncased")

#     # Show the model again after deleting.
#     temp = textModelsPy()
#     for a in temp:
#         print(a)
