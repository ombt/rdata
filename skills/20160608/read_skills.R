cert <- read_csv_skills("SkillsReport-20160608-Certification.csv")
orig.cert <- cert

cert$Grp <- cert$Group
cert$Group <- NULL

comp <- read_csv_skills("SkillsReport-20160608-Competency.csv")
orig.comp <- comp

tool <- read_csv_skills("SkillsReport-20160608-NewTools.csv")
orig.tool <- tool

tool$ToolName <- tool$Tool.Name
tool$Tool.Name <- NULL

# summ <- read_csv_skills("SkillsReport-20160608-Summary.csv")

