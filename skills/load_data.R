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

