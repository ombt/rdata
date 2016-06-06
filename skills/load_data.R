read_csv_skills <- function(csv_fname,sep=";")
{
    if ((csv_fname == "") | is.na(csv_fname))
    {
        stop("CSV file names not set or zero-length")
    }
    #
    csv_path = Sys.getenv("OMBT_CSV_BASE_PATH")
    if ((csv_path == "") | is.na(csv_path))
    {
        stop("OMBT_CSV_BASE_PATH not set or zero-length")
    }
    csv_path = paste(csv_path, "skills", sep="/")
    #
    return(read_csv_file(csv_path, csv_fname, separator=sep))
}


# cert <- read_csv_skills("SkillsReport-Certification-2.csv")
# comp <- read_csv_skills("SkillsReport-Competency-2.csv")
# tool <- read_csv_skills("SkillsReport-NewTools-2.csv")
# summ <- read_csv_skills("SkillsReport-Summary-2.csv")

#
# sqlite_load_cicor_20151013 <- function(nrows=0)
# {
    # return(sqlite_load_u01("cicor_20151013",nrows))
# }

# sqlite_load_nv_cicor_20151013 <- function(nrows=0)
# {
    # return(sqlite_load_nv_u01("cicor_20151013",nrows))
# }

# #
# sqlite_load_cicor_20151013 <- function(nrows=0)
# {
#     db_path = Sys.getenv("OMBT_DB_BASE_PATH")
#     if ((db_path == "") | is.na(db_path))
#     {
#         stop("OMBT_DB_BASE_PATH not set or zero-length")
#     }
#     db_name = "cicor_20151013"
#     #
#     db = sqlite_open_db(db_path, db_name)
#     #
#     tbls = c("FILENAME_TO_IDS",
#              "InspectionData",
#              "MountPickupFeeder",
#              "Count",
#              "MountPickupNozzle",
#              "CycleTime",
#              "Index",
#              "FID_DATA",
#              "Information",
#              "Time")
#     #
#     data = list()
#     #
#     for (tbl in tbls)
#     {
#         print(paste("reading table", tbl))
#         data[[tbl]] = sqlite_load_table_from_db(db,
#                                                 tbl,
#                                                 nrows=nrows)
#     }
#     #
#     sqlite_close_db(db)
#     #
#     return(data)
# }
# 
# sqlite_load_nv_cicor_20151013 <- function(nrows=0)
# {
#     print(paste("entry at", Sys.time()))
#     #
#     db_path = Sys.getenv("OMBT_DB_BASE_PATH")
#     if ((db_path == "") | is.na(db_path))
#     {
#         stop("OMBT_DB_BASE_PATH not set or zero-length")
#     }
#     db_name = "cicor_20151013"
#     #
#     print(paste("open db at", Sys.time()))
#     db = sqlite_open_db(db_path, db_name)
#     #
#     tbls = c("FILENAME_TO_IDS",
#              "MountPickupFeeder",
#              "MountPickupNozzle",
#              "FID_DATA")
#     #
#     nv_tbls = c("InspectionData",
#                 "Count",
#                 "CycleTime",
#                 "Index",
#                 "Information",
#                 "Time")
#     #
#     data = list()
#     #
#     print(paste("start reading tbls at", Sys.time()))
#     for (tbl in tbls)
#     {
#         print(paste("reading NON-NV table", tbl, "at", Sys.time()))
#         data[[tbl]] = sqlite_load_table_from_db(db,
#                                                 tbl,
#                                                 nrows=nrows)
#     }
#     for (tbl in nv_tbls)
#     {
#         print(paste("reading NV table", tbl, "at", Sys.time()))
#         data[[tbl]] = sqlite_load_nv_table_from_db(db,
#                                                    tbl,
#                                                    nrows=nrows)
#     }
#     #
#     print(paste("close db at", Sys.time()))
#     sqlite_close_db(db)
#     #
#     print(paste("done at", Sys.time()))
#     return(data)
# }