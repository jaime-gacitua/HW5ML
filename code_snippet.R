# ---------------------------------------------------------------------------------------------
movies <- read.table("u.item", sep = "|", header = FALSE, stringsAsFactors = FALSE, quote="")
movies <- movies[,c(1,2)]
names(movies) <- c("movieid","movie")

rank   <- read.table("u.data", sep = "\t", header = FALSE, stringsAsFactors = FALSE,
                     col.names = c("userid","movieid","rating","ts"))

critics <- merge(movies, rank, by = "movieid")
critics$movie <- NULL
critics$ts <- NULL
names(critics) <- c("movieid","person","rank")



