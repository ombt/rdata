cert <- read_csv_skills("SkillsReport-20160607-Certification.csv")

comp <- read_csv_skills("SkillsReport-20160607-Competency.csv")

tool <- read_csv_skills("SkillsReport-20160607-NewTools.csv")

tool$ToolName <- tool$Tool.Name
tool$Tool.Name <- NULL

# summ <- read_csv_skills("SkillsReport-20160607-Summary.csv")

