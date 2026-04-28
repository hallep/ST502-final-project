import pandas as pd

def get_xpt(name:str, cols:list[str]) -> pd.DataFrame:
    return pd.read_sas(f"https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/{name}_J.XPT", format="xport", index="SEQN")[cols]

# Get columns from datasets
xpts = [
    get_xpt("DEMO", ["RIAGENDR", "RIDAGEYR", "RIDRETH3", "DMDEDUC2", "DMDMARTL", "INDFMPIR"]),
    get_xpt("ALQ", ["ALQ111", "ALQ121"]),
    get_xpt("BPX", ["BPXPLS", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4", "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"]),
    get_xpt("DBQ", ["DBQ700"]),
    get_xpt("PAQ", ["PAQ605", "PAQ610", "PAD615", "PAQ620", "PAQ625", "PAD630", "PAQ650", "PAQ655", "PAD660", "PAQ665", "PAQ670", "PAD675", "PAD680"]),
    get_xpt("SLQ", ["SLD012", "SLD013"]),
    get_xpt("SMQ", ["SMQ040"]),
    get_xpt("BPQ", ["BPQ020"]),
    get_xpt("CDQ", ["CDQ001"]),
    get_xpt("DIQ", ["DIQ010"]),
    get_xpt("MCQ", ["MCQ160C", "MCQ160E", "MCQ160F", "MCQ160M", "MCQ160G", "MCQ160O", "MCQ160L"]),
    get_xpt("BMX", ["BMXBMI"]),
    get_xpt("TCHOL", ["LBXTC"]),
    get_xpt("FERTIN", ["LBXFER"]),
]

# Combine datasets
data = pd.concat(xpts, axis=1)
data.index = pd.Index(data.index.values.astype(int), name="SEQN")

# save raw data
data.to_csv("NHANES_2017_data.txt", sep="\t", na_rep="nan")
print(data)

# blood pressure
data["BPXSY"] = data[["BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4"]].median(axis=1, skipna=True)
data["BPXDI"] = data[["BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"]].median(axis=1, skipna=True)

# remove subjects < 24 years old
data = data[data["RIDAGEYR"] >= 24]

# Clean data for regression
variables = {
    "gender" : (data["RIAGENDR"] == 2).astype(int),
    "age" : data["RIDAGEYR"],

    "mexAmer" : (data["RIDRETH3"] == 1).astype(int),
    "hispanic" : (data["RIDRETH3"] == 2).astype(int),
    "black" : (data["RIDRETH3"] == 4).astype(int),
    "asian" : (data["RIDRETH3"] == 6).astype(int),
    "otherRace" : (data["RIDRETH3"] == 7).astype(int),

    "someHS" : (data["DMDEDUC2"] == 2).astype(int),
    "HSGrad" : (data["DMDEDUC2"] == 3).astype(int),
    "someCollege" : (data["DMDEDUC2"] == 4).astype(int),
    "collegeGrad" : (data["DMDEDUC2"] == 5).astype(int),

    "married" : (data["DMDMARTL"] == 1).astype(int),
    "widowed" : (data["DMDMARTL"] == 2).astype(int),
    "divorced" : (data["DMDMARTL"] == 3).astype(int),
    "separated" : (data["DMDMARTL"] == 4).astype(int),
    "livingWithPartner" : (data["DMDMARTL"] == 6).astype(int),

    "incomePoverty" : data["INDFMPIR"],
}

# alcohol
variables.update({
    "dailyAlc" : data["ALQ121"].isin([1]).astype(int),
    "weeklyAlc" : data["ALQ121"].isin([2, 3, 4, 5]).astype(int),
    "monthlyAlc" : data["ALQ121"].isin([6, 7]).astype(int),
    "yearlyAlc" : data["ALQ121"].isin([8,  9, 10]).astype(int),
})

# diet
variables.update({
    "excellentDiet" : (data["DBQ700"] == 1).astype(int),
    "veryGoodDiet" : (data["DBQ700"] == 2).astype(int),
    "goodDiet" : (data["DBQ700"] == 3).astype(int),
    "fairDiet" : (data["DBQ700"] == 4).astype(int),
})

# physical activity
def sort_pa(cBin:str, cDays:str) -> pd.Series:
    return data[[cBin, cDays]].apply(lambda x: 0 if (x[cBin] == 2) else x[cDays], axis=1)

variables.update({
    "vigWork" : sort_pa("PAQ605", "PAQ610"),
    "modWork" : sort_pa("PAQ620", "PAQ625"),
    "vigRec" : sort_pa("PAQ650", "PAQ655"),
    "modRec" : sort_pa("PAQ665", "PAQ670"),
    "sedentary" : data["PAD680"] / 60,
})

# smoking
variables.update({
    "smokeEveryDay" : (data["SMQ040"] == 1).astype(int),
    "smokeSomeDays" : (data["SMQ040"] == 2).astype(int),
})

# numerical
numerical = {
    "SLD012" : "sleepWeekdays",
    "SLD013" : "sleepWeekends",
    "BPXPLS" : "pulse",
    "BPXSY" : "systolicBP",
    "BPXDI" : "diastolicBP",
    "BMXBMI" : "bmi",
    "LBXTC" : "cholesterol",
    "LBXFER" : "ferritin",
}
variables.update({v : data[k] for k,v in numerical.items()})

# binary indicators
binary = {
    "BPQ020" : "highBP", 
    "CDQ001" : "chestPain", 
    "DIQ010" : "diabetes", 
    "MCQ160C" : "coronaryHD", 
    "MCQ160E" : "heartAttack", 
    "MCQ160F" : "stroke", 
    "MCQ160M" : "thyroidProblem", 
    "MCQ160G" : "emphysema", 
    "MCQ160O" : "COPD", 
    "MCQ160L" : "liverCondition"
}
variables.update({v : (data[k] == 1).astype(int) for k,v in binary.items()})

# compile and save
vars = pd.DataFrame(variables, index=pd.Index(data.index.values, name="id")).dropna(axis=0)
vars.to_csv("regression_variables.txt", sep="\t", na_rep="nan")

print(vars)
