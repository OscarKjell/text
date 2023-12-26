# -*- coding: utf-8 -*-
import os, glob, re
#import json
#from collections import OrderedDict
from transformers import TRANSFORMERS_CACHE
#from tokenizers import Tokenizer
#from transformers import AutoTokenizer, AutoModelForMaskedLM  # for test only

#modelRegex = "huggingface\.co\/(.*)(pytorch_model\.bin$|resolve\/main\/tf_model\.h5$)"
modelRegex = "(.*)(.model$|pytorch_model.bin$|tf_model.h5$)"
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


    if "/" in target:
        target1 = target.replace("/", "--")
    else: target1 = target
    deleteModel = []
    for folder in metaFolders:
        if target1 in folder:
            deleteModel.append(folder)
    if len(deleteModel) != 0:
        for folder in deleteModel:
            if os.path.exists(folder): 
                os.remove(folder)
                print("Model " + target + " is removed!")
            else: print("Fail to remove model " + target + "!")
    else:
        print("Model(s) " + target + " is/are not found!")

    return 0

# main for test
#if __name__ == '__main__':

#     textModelsRMPy(target="distilbert-base-uncased")
#     # Show the downloaded model.
#     temp = textModelsPy()
#     for a in temp:
#         print(a)

#     tokenizer = AutoTokenizer.from_pretrained("distilbert-base-uncased")
#     model = AutoModelForMaskedLM.from_pretrained("distilbert-base-uncased")
#     textModelsRMPy(target="distilbert-base-uncased")

     # Show the model again after deleting.
#     temp = textModelsPy()
#     for a in temp:
#         print(a)
