import pandas as pd

df = pd.read_csv("regression_variables.txt", sep="\t", index_col=0)

df["gender"] = df["gender"].apply(lambda x: "female" if x else "male")
df["race"] = df.apply(lambda x: "black" if x["black"] else "mexAmer" if x["mexAmer"] else "hispanic" if x["hispanic"] else "asian" if x["asian"] else "other" if x["otherRace"] else "white", axis=1)
df["edu"] = df.apply(lambda x: "someHS" if x["someHS"] else "HSGrad" if x["HSGrad"] else "someCollege" if x["someCollege"] else "collegeGrad" if x["collegeGrad"] else "noHS", axis=1)
df["marital"] = df.apply(lambda x: "married" if x["married"] else "widowed" if x["widowed"] else "divorced" if x["divorced"] else "separated" if x["separated"] else "partner" if x["livingWithPartner"] else "single", axis=1)
df["alcohol"] = df.apply(lambda x: "daily" if x["dailyAlc"] else "weekly" if x["weeklyAlc"] else "monthly" if x["monthlyAlc"] else "yearly" if x["yearlyAlc"] else "none", axis=1)
df["smoke"] = df.apply(lambda x: "everyday" if x["smokeEveryDay"] else "somedays" if x["smokeSomeDays"] else "never", axis=1)
df["diet"] = df.apply(lambda x: "excellent" if x["excellentDiet"] else "veryGood" if x["veryGoodDiet"] else "good" if x["goodDiet"] else "fair" if x["fairDiet"] else "poor", axis=1)
df["diabetes"] = df["diabetes"].apply(lambda x: "yes" if x else "no")
df["thyroid"] = df["thyroidProblem"].apply(lambda x: "yes" if x else "no")

def group_cols(col:int):
    c = df.groupby([col, "highBP"]).count()["age"].unstack("highBP")
    p = c.divide(c.sum(axis=1), axis=0)
    b = pd.concat((c, p), axis=1)
    print(b)
    b.columns = ["no_count", "yes_count", "no_prop", "yes_prop"]
    b = b[["no_count", "no_prop", "yes_count", "yes_prop"]]
    return b

vars = ["gender", "race", "edu", "marital", "alcohol", "smoke", "diet", "diabetes", "thyroid"]

rows = [("gender", c) for c in ["male", "female"]]
rows += [("race", c) for c in ["white", "black", "mexAmer", "hispanic", "asian", "other"]]
rows += [("edu", c) for c in ["noHS", "someHS", "HSGrad", "someCollege", "collegeGrad"]]
rows += [("marital", c) for c in ["single", "married", "widowed", "divorced", "separated", "partner"]]
rows += [("alcohol", c) for c in ["none", "daily", "weekly", "monthly", "yearly"]]
rows += [("smoke", c) for c in ["never", "everyday", "somedays"]]
rows += [("diet", c) for c in ["poor", "fair", "good", "veryGood", "excellent"]]
rows += [("diabetes", c) for c in ["no", "yes"]]
rows += [("thyroid", c) for c in ["no", "yes"]]

d = pd.concat([group_cols(c) for c in vars], keys=vars).loc[rows]
print(d)

