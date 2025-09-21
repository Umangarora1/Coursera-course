library(dplyr)

features   <- read.table("UCI HAR Dataset/features.txt",
                         col.names = c("idx", "feature"),
                         stringsAsFactors = FALSE)

activities <- read.table("UCI HAR Dataset/activity_labels.txt",
                         col.names = c("Code", "Activity"),
                         stringsAsFactors = FALSE)

read_split <- function(split) {
  subj <- read.table(file.path("UCI HAR Dataset", split, paste0("subject_", split, ".txt")),
                     col.names = "Subject")
  X    <- read.table(file.path("UCI HAR Dataset", split, paste0("X_", split, ".txt")),
                     col.names = features$feature)
  y    <- read.table(file.path("UCI HAR Dataset", split, paste0("y_", split, ".txt")),
                     col.names = "Code")
  bind_cols(subj, y, X)
}

# --- 3) Assemble full dataset ---
all_data <- bind_rows(
  read_split("train"),
  read_split("test")
)

mean_std <- all_data %>%
  select(Subject, Code, matches("mean|std"))


mean_std <- mean_std %>%
  left_join(activities, by = "Code") %>%
  relocate(Activity, .after = Code)

clean_names <- function(nm) {
  nm <- gsub("^t", "Time", nm)
  nm <- gsub("^f", "Frequency", nm)
  nm <- gsub("Acc", "Accelerometer", nm)
  nm <- gsub("Gyro", "Gyroscope", nm)
  nm <- gsub("Mag", "Magnitude", nm)
  nm <- gsub("BodyBody", "Body", nm)
  nm
}
names(mean_std) <- clean_names(names(mean_std))

final_data <- mean_std %>%
  select(-Code) %>%                              
  group_by(Subject, Activity) %>%
  summarise(across(where(is.numeric), mean),     
            .groups = "drop")

write.table(final_data, "FinalData.txt", row.names = FALSE)
