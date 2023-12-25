# -*- coding: utf-8 -*-
import os, glob, re
#import json
#from collections import OrderedDict
from transformers import TRANSFORMERS_CACHE
from tokenizers import Tokenizer
from transformers import AutoTokenizer, AutoModelForMaskedLM  # for test only

#modelRegex = "huggingface\.co\/(.*)(pytorch_model\.bin$|resolve\/main\/tf_model\.h5$)"
modelRegex = "(.*)(pytorch_model.bin$|tf_model.h5$)"
tokenizerRegex = "(tokenizer.json$)"


def fileFinder(folder, pattern):
    
    modelRegex = re.compile(pattern)
    fileNames = []
    
    for root, dirs, files in os.walk(folder):
        for basename in files:
            if modelRegex.match(basename):
                filename = os.path.join(root, basename)
                fileNames.append(filename)

    return True if len(fileNames) > 0 else False

def folder_2_modelNam(folder):
    
    pos = folder.find("models--")
    A = folder[pos+len("models--"):]
    
    if "--" in A:
        A = A.replace("--", "/")

    return A

def writeNamesTransformers(folder, cachedModels, cachedTokenizers):

    modelName = folder_2_modelNam(folder)
    
    if fileFinder(folder, modelRegex):
        cachedModels.append(modelName)
    if fileFinder(folder, tokenizerRegex):
        cachedTokenizers.append(modelName)

    return cachedModels, cachedTokenizers
    

def textModelsPy():
    
    metaFolders = glob.glob(TRANSFORMERS_CACHE + '/models--*')
    #print(f"metaFiles: \n'): {metaFolders}")

    
    cachedModels = []
    cachedTokenizers = []
                
    for folder in metaFolders:
        cachedModels, cachedTokenizers = writeNamesTransformers(
                                             folder
                                             ,cachedModels
                                             ,cachedTokenizers
                                             )   
    
    #if cachedTokenizers:
    #    cachedTokenizers = OrderedDict(sorted(cachedTokenizers.items(), key=lambda k: k[0]))

    if cachedModels and not cachedTokenizers:
        returnTarget = (list(sorted(cachedModels)), ("NoTokenizersAvailable"))
        return tuple(returnTarget)
    elif not cachedModels and cachedTokenizers:
        returnTarget = (("NoModelsAvailable"), list(sorted(cachedTokenizers)))
        return tuple(returnTarget)
    elif not cachedModels and not cachedTokenizers:
        returnTarget = (("NoModelsAvailable"),("NoTokenizersAvailable"))
        return tuple(returnTarget)
    else:
        returnTarget = (list(sorted(cachedModels)), list(sorted(cachedTokenizers)))
        return tuple(returnTarget)

def textModelsRMPy(target="default"):

    # transformerLists = textModelsPy()
    metaFolders = glob.glob(TRANSFORMERS_CACHE + '/models--*')

    deleteModel = []
    for folder in metaFiles:
        for target1 in target:
            if "/" in target1:
                target1 = target1.replace("/", "--")
            if target1 in folder:
                deleteModel.append(target1)
    if len(deleteModel) != 0:
        for target1 in deleteModel:
            if os.path.exists(target1): 
                os.remove(target1)
                print("Model " + target1 + " is removed!")
    else:
        print("Model(s) " + target + " is/are not found!")
        
        #with open(file) as j:
        #    data = json.load(j)
        #    if isinstance(target, str):
        #        if data['url'].find(target) != -1:
        #            toDelete = file.find(".json")
        #            targetFind += 1
        #if toDelete != 0:
        #    if os.path.exists(file[0:toDelete]): os.remove(file[0:toDelete])
        #    if os.path.exists(file[0:toDelete] + ".json"): os.remove(file[0:toDelete] + ".json")
        #    if os.path.exists(file[0:toDelete] + ".lock"): os.remove(file[0:toDelete] + ".lock")
        #    toDelete = 0
    #if targetFind == 0:
       # print("Model(s) " + target + " do/does not exist!")

    return 0

# main for test
if __name__ == '__main__':

     textModelsRMPy(target="distilbert-base-uncased")
     # Show the downloaded model.
     temp = textModelsPy()
     for a in temp:
         print(a)

     tokenizer = AutoTokenizer.from_pretrained("distilbert-base-uncased")
     model = AutoModelForMaskedLM.from_pretrained("distilbert-base-uncased")
     textModelsRMPy(target="distilbert-base-uncased")

     # Show the model again after deleting.
     temp = textModelsPy()
     for a in temp:
         print(a)
