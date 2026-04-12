import pandas as pd

def get_xpt(name:str, cols:list[str]) -> pd.DataFrame:
    return pd.read_sas(f"https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/{name}_J.XPT", format="xport", index="SEQN")[cols]

# Get columns from datasets
xpts = dict(
    demo = get_xpt("DEMO", ["RIAGENDR", "RIDAGEYR", "RIDRETH3", "DMDEDUC2", "DMDMARTL", "INDFMPIR"]),
    alq = get_xpt("ALQ", ["ALQ111", "ALQ121"]),
    dbq = get_xpt("DBQ", ["DBQ700"]),
    paq = get_xpt("PAQ", ["PAQ605", "PAQ610", "PAD615", "PAQ620", "PAQ625", "PAD630", "PAQ650", "PAQ655", "PAD660", "PAQ665", "PAQ670", "PAD675", "PAD680"]),
    slq = get_xpt("SLQ", ["SLD012", "SLD013"]),
    bpq = get_xpt("BPQ", ["BPQ020"]),
    cdq = get_xpt("CDQ", ["CDQ001"]),
    diq = get_xpt("DIQ", ["DIQ010"]),
    mcq = get_xpt("MCQ", ["MCQ160C", "MCQ160E", "MCQ160F", "MCQ160M", "MCQ160G", "MCQ160O", "MCQ160L"]),
    bmx = get_xpt("BMX", ["BMXBMI"]),
    tchol = get_xpt("TCHOL", ["LBXTC"]),
    fertin = get_xpt("FERTIN", ["LBXFER"]),
    folate = get_xpt("FOLATE", ["LBDRFO"]),
    ins = get_xpt("INS", ["LBXIN", "LBDINLC"]),
)

# blood pressure
bpx = get_xpt("BPX", ["BPXCHR", "BPXPLS", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4", "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"])
bpx["BPXSY"] = bpx[["BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4"]].median(axis=1, skipna=True)
bpx["BPXDI"] = bpx[["BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"]].median(axis=1, skipna=True)
xpts["bpx"] = bpx[["BPXCHR", "BPXPLS", "BPXSY", "BPXDI"]]

# Combine datasets
data = pd.concat(xpts.values(), axis=1)
data.index = pd.Index(data.index.values.astype(int), name="SEQN")

# remove subjects < 20 years old
data = data[data["RIDAGEYR"] >= 20]

# save raw data
data.to_csv("NHANES_2017_data.txt", sep="\t")
print(data)

# Clean data for regression
variables = {
    "gender" : (data["RIAGENDR"] == 2).astype(int),
    "age" : data["RIDAGEYR"],

    "mexAmer" : (data["RIDRETH3"] == 1).astype(int),
    "hispanic" : (data["RIDRETH3"] == 2).astype(int),
    "black" : (data["RIDRETH3"] == 4).astype(int),
    "asian" : (data["RIDRETH3"] == 6).astype(int),
    "otherRace" : (data["RIDRETH3"] == 7).astype(int),

    "noHS" : (data["DMDEDUC2"] == 1).astype(int),
    "someHS" : (data["DMDEDUC2"] == 2).astype(int),
    "HSGrad" : (data["DMDEDUC2"] == 4).astype(int),
    "someCollege" : (data["DMDEDUC2"] == 6).astype(int),
    "collegeGrad" : (data["DMDEDUC2"] == 7).astype(int),

    "married" : (data["DMDMARTL"] == 1).astype(int),
    "widowed" : (data["DMDMARTL"] == 2).astype(int),
    "divorced" : (data["DMDMARTL"] == 3).astype(int),
    "separated" : (data["DMDMARTL"] == 4).astype(int),
    "livingWithPartner" : (data["DMDMARTL"] == 6).astype(int),

    "incomePoverty" : data["INDFMPIR"],
}

# binary indicators
binary = {
    "BPQ020" : "highBP", 
    "CDQ001" : "chestPain", 
    "DIQ010" : "diabetes", 
    "MCQ160C" : "CoronaryHD", 
    "MCQ160E" : "heartAttack", 
    "MCQ160F" : "stroke", 
    "MCQ160M" : "thyroidProblem", 
    "MCQ160G" : "emphysema", 
    "MCQ160O" : "COPD", 
    "MCQ160L" : "liverCondition"
}
variables.update({v : (data[k] == 1).astype(int) for k,v in binary.items()})

# alcohol
variables.update({
    "dailyAlc" : data["ALQ121"].isin([1]).astype(int),
    "weeklyAlc" : data["ALQ121"].isin([2, 3, 4, 5]).astype(int),
    "monthlyAlc" : data["ALQ121"].isin([6, 7]).astype(int),
    "yearlyAlc" : data["ALQ121"].isin([8,  9, 10]).astype(int),
})

# diet
variables.update({
    "diet2" : (data["DBQ700"] == 2).astype(int),
    "diet3" : (data["DBQ700"] == 3).astype(int),
    "diet4" : (data["DBQ700"] == 4).astype(int),
    "diet5" : (data["DBQ700"] == 5).astype(int),
})

# physical activity
def sort_pa(cBin:str, cDays:str, cMin:str) -> pd.Series:
    return data[[cBin, cDays]].apply(lambda x: 0 if (x[cBin] == 2) else x[cDays], axis=1)

variables.update({
    "vigWork" : sort_pa("PAQ605", "PAQ610", "PAD615"),
    "modWork" : sort_pa("PAQ620", "PAQ625", "PAD630"),
    "vigRec" : sort_pa("PAQ650", "PAQ655", "PAD660"),
    "modRec" : sort_pa("PAQ665", "PAQ670", "PAD675"),
    "sedent" : data["PAD680"],
})

# numerical
numerical = {
    "SLD012" : "sleepWeekdays",
    "SLD013" : "sleepWeekends",
    "BPXCHR" : "heartrate",
    "BPXPLS" : "pulse",
    "BPXSY" : "systolicBP",
    "BPXDI" : "diastolicBP",
    "BMXBMI" : "BMI",
    "LBXTC" : "cholesterol",
    "LBXFER" : "ferritin",
    "LBDRFO" : "folate",
    "LBXIN" : "insulin",
}
variables.update({v : data[k] for k,v in numerical.items()})

# compile and save
vars = pd.DataFrame(variables, index=pd.Index(data.index.values, name="id"))
vars.to_csv("regression_variables.txt", sep="\t")

print(vars)
