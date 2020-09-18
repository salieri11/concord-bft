from pathlib import Path
import yaml
import re, sys, os
import json


JSON_PATH = "./data.json"

def readMetrics():
    with open(r'extract_metrics_conf.yml') as file:
        metricsList = yaml.load(file, Loader=yaml.FullLoader)
        return metricsList

def deleteFiles():
    if os.system("rm -rf ./data.json") != 0:
        print("Delete file failed",  "./data.json")

def processLogs(key, delimiter, subdelimiter, pattern, pos, subPos, logpath):
    mainDict = {}
    mList = [[]]
    try:
        if os.stat(logpath).st_size == 0:
            print("Error: Log file is empty")
            return
        f = open(logpath, "r")
    except IOError:
        print("Error: Could not open file!")
        return
    for line in f:
        allKeyMatch = True
        for i in range(len(pattern)):
            match = re.search(pattern[i], line)
            if match == None:
                allKeyMatch = False

        if allKeyMatch:
            curline = line.split(delimiter)
            ts = curline[0]
            curlist = curline[pos[0]].split(subdelimiter)
            if subPos != None:
                mList.append((ts, curlist[subPos[0]]))
            else:
                mList.append(curline[pos[0]])
    mainDict.setdefault(key, mList)
    with open(JSON_PATH, 'a', encoding='utf-8') as f:
        json.dump(mainDict, f, ensure_ascii=False, indent=4)

def readLogs(metricsList):
    metricsName = metricsList.keys()
    delimiter = ""
    subdelimiter = ""
    logpath = ""
    for name in metricsName:
        metricsDetails = metricsList[name]
        if name == "delimiter":
            delimiter = metricsList[name]
        elif name == "subdelimiter":
            subdelimiter = metricsList[name]
        elif name == "logpath":
            logpath = metricsList[name]
        else:
            metricsDetails = metricsList[name]
            if len(metricsDetails) != 3:
                print("Required \"Keyword\" and \"Metrics index position as list\" for "+ name)
                return
            pattern = ""
            index = None
            subIndex=  None
            for key in metricsDetails:
                if key == "Keyword":
                    pattern = metricsDetails[key]
                elif key == "Value Index":
                    index = metricsDetails[key]
                elif key == "Value SubIndex":
                    subIndex = metricsDetails[key]
            if name == "" or delimiter == "" or subdelimiter == "" or pattern == "" or index == None:
                print("Configuration file format is invalid")
                return
            if not os.path.exists(logpath):
                print(logpath + "does not exists");
                return
            processLogs(name, delimiter, subdelimiter, pattern, index, subIndex, logpath)
                    

def main():
    deleteFiles()
    metricsList =  readMetrics()
    readLogs(metricsList)
    
if __name__ == "__main__":
    main()
