import pandas as pd

def get_xpt(name:str, cols:list[str]) -> pd.DataFrame:
    return pd.read_sas(f"https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/{name}_J.XPT", format="xport", index="SEQN")[cols]

xpts = dict(
    demo = get_xpt("DEMO", ["RIAGENDR", "RIDAGEYR", "RIDRETH3", "DMDEDUC2", "DMDMARTL", "INDFMPIR"]),
    diq = get_xpt("DIQ", ["DIQ010"]),
    mcq = get_xpt("MCQ", ["MCQ160C", "MCQ160E", "MCQ160F", "MCQ160M", "MCQ160G", "MCQ160O", "MCQ160L"]),
    alq = get_xpt("ALQ", ["ALQ111", "ALQ130"]),
    bpq = get_xpt("BPQ", ["BPQ020"]),
    cdq = get_xpt("CDQ", ["CDQ001"]),
    dbq = get_xpt("DBQ", ["DBQ700"]),
    paq = get_xpt("PAQ", ["PAD660", "PAD675", "PAD680"]),
    slq = get_xpt("SLQ", ["SLD012", "SLD013"]),
    bmx = get_xpt("BMX", ["BMXBMI"]),
    tchol = get_xpt("TCHOL", ["LBXTC"]),
    fertin = get_xpt("FERTIN", ["LBXFER"]),
    folate = get_xpt("FOLATE", ["LBDRFO"]),
    ins = get_xpt("INS", ["LBXIN", "LBDINLC"]),
)

bin_cols = ["RIAGENDR", "RIDAGEYR", "RIDRETH3", "DMDEDUC2", "DMDMARTL",
            "DIQ010", "MCQ160C", "MCQ160E", "MCQ160F", "MCQ160M", "MCQ160G", "MCQ160O", "MCQ160L",
            "ALQ111", "BPQ020", "CDQ001", "DBQ700", "PAD660", "PAD675", "PAD680", "SLD012", "SLD013"]

# blood pressure
bpx = get_xpt("BPX", ["BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4", "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"])
bpx["BPXSY"] = bpx[["BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4"]].mean(axis=1)
bpx["BPXDI"] = bpx[["BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"]].mean(axis=1)
xpts["bpx"] = bpx[["BPXSY", "BPXDI"]]

# combine
data = pd.concat(xpts.values(), axis=1)
data.index = pd.Index(data.index.values.astype(int))

# subset
data = data[data["RIDAGEYR"] >= 20]

data.to_csv("NHANES_2017_data.txt", sep="\t", na_rep="NaN", index_label="SEQN")

print(data)
