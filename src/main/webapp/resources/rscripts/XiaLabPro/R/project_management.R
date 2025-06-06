SetProjectDbPath <- function(path){
    projectPath <<- path;
    projectDbPath <<- paste0(path, "/", "omicsquare_account.sqlite");
}

CreateProjectDb <- function() {
  library(RSQLite)
  library(DBI)

  # Establish SQLite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
  
  # Error handling with tryCatch
  tryCatch({
    dbExecute(con, "PRAGMA journal_mode = wal2");
    dbExecute(con, "PRAGMA busy_timeout=5000");
    # Check and create 'users' table if it doesn't exist
    if (!dbExistsTable(con, "users")) {
      create_table_query <- "
        CREATE TABLE users (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          password TEXT,
          email TEXT UNIQUE,
          firstname TEXT,
          lastname TEXT,
          institution TEXT,
          activateCode TEXT,
          expDate TEXT,
          activated INTEGER DEFAULT 0
        )"
      dbExecute(con, create_table_query)
    }else{
    # Check if the columns are missing
    missing_columns_query <- "
      PRAGMA table_info(users);
    "
    existing_columns <- dbGetQuery(con, missing_columns_query)

      if (!("activateCode" %in% existing_columns$name)) {
        alter_table_query1 <- "ALTER TABLE users ADD COLUMN activateCode TEXT;"
        dbExecute(con, alter_table_query1)
      }

      if (!("expDate" %in% existing_columns$name)) {
        alter_table_query2 <- "ALTER TABLE users ADD COLUMN expDate TEXT;"
        dbExecute(con, alter_table_query2)
      }

      if (!("activated" %in% existing_columns$name)) {
        alter_table_query3 <- "ALTER TABLE users ADD COLUMN activated INTEGER DEFAULT 0;"
        dbExecute(con, alter_table_query3)
      }
    }

# Step 1: Create a backup table with the modified schema
create_backup_table_query <- "
  CREATE TABLE users_backup (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    password TEXT,
    email TEXT UNIQUE,
    firstname TEXT,
    lastname TEXT,
    institution TEXT,
    activateCode TEXT,
    expDate TEXT,
    activated INTEGER DEFAULT 0
  );"
dbExecute(con, create_backup_table_query)

# Step 2: Copy data from the original table to the backup table
copy_data_query <- "INSERT INTO users_backup SELECT * FROM users;"
dbExecute(con, copy_data_query)

# Step 3: Drop the original table
drop_original_table_query <- "DROP TABLE users;"
dbExecute(con, drop_original_table_query)

# Step 4: Rename the backup table to the original table name
rename_backup_table_query <- "ALTER TABLE users_backup RENAME TO users;"
dbExecute(con, rename_backup_table_query)


    # Check if table exists, if not create it
    if(!"project" %in% dbListTables(con)) {
      query <- "CREATE TABLE project (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                userId TEXT,
                name TEXT,
                description TEXT,
                module TEXT,
                moduleReadable TEXT,
                dataType TEXT,
                date TEXT,
                folderName TEXT,
                javaHistory TEXT,
                naviString TEXT,
                naviCode TEXT,
                org TEXT,
                partialToken TEXT,
                toolName TEXT,
                toolCode TEXT, 
                projectType TEXT,
                paired BOOLEAN,
                regression BOOLEAN
        )"
      dbExecute(con, query)
    }
  }, error = function(e) {
    message("Error: ", e$message)
  }, finally = {
    # Close the connection
    dbDisconnect(con)
  })
}

RegisterUser <- function(password, email, firstname, lastname, institution) {
  library(RSQLite);
  library(DBI);
  
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath);
    dbBegin(con)  # Start a transaction
    
    # Hash the password
    hashed_password <- digest::digest(password, algo = "sha256")
    
    # Check if username or email already exists
    check_query <- sprintf("SELECT COUNT(*) FROM users WHERE email = '%s'", email)
    existing_count <- dbGetQuery(con, check_query)$`COUNT(*)`

    if(existing_count > 0) {
      dbRollback(con)  # Roll back the transaction
      return("Username or email already exists.")
    }
    
    # Insert new user
    query <- sprintf("INSERT INTO users (password, email, firstname, lastname, institution, activated) VALUES ('%s', '%s', '%s', '%s', '%s', 0)", hashed_password, email, firstname, lastname, institution)
    dbExecute(con, query)
    
    dbCommit(con)  # Commit the transaction
    return("User registered successfully.")
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbRollback(con)  # Roll back the transaction in case of an error
      dbDisconnect(con)  # Disconnect if connection was actually established
    }
    return(paste0("Registration Error - ", e$message))
  })
}


LoginUser <- function(password, email) {
  library(RSQLite);
  library(DBI);
  print(projectDbPath);

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath);
    dbBegin(con)  # Start a transaction
    
    # Hash the password
    hashed_password <- digest::digest(password, algo = "sha256")
    
    # Fetch stored password and additional fields
    query <- sprintf("SELECT * FROM users WHERE email = '%s'", email)
    user_data <- dbGetQuery(con, query)
    
    if(nrow(user_data) == 0) {
      dbRollback(con)  # Roll back the transaction
      return(c("Username not found."))
    }

    
    stored_password <- user_data$password[1]
    activation_status <- user_data$activated[1]
    
    if (activation_status != 1) {
      dbRollback(con)  # Roll back the transaction
      return(c("Account needs to be activated first."))
    }

    if(stored_password == hashed_password) {
      dbCommit(con)  # Commit the transaction
      # Return the additional fields as a list
      return(c(user_data$email[1], user_data$firstname[1], user_data$lastname[1], user_data$institution[1]))
    } else {
      dbRollback(con)  # Roll back the transaction
      return(c("Invalid password."))
    }
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbRollback(con)  # Roll back the transaction in case of an error
      dbDisconnect(con)  # Disconnect if connection was actually established
    }
    return(c(paste0("Login Error - ", e$message)))
  })
}

writeProjectToSQLite <- function(uid, name, description, module, moduleReadable, dataType, date,
                                 folderName, javaHistory, naviString, currentNaviCode,
                                 idNum, org, partialToken, toolName, toolCode, projectType, paired, regression) {

  javaHistory <- javaHistoryString;
  
  # Load necessary libraries
  library(RSQLite)
  library(DBI)
  
  result <- 0
  
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), projectDbPath)
    
    dbBegin(con)
    
    # Check if table exists, if not create it
    if(!"project" %in% dbListTables(con)) {
      query <- "CREATE TABLE project (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                userId TEXT,
                name TEXT,
                description TEXT,
                module TEXT,
                moduleReadable TEXT,
                dataType TEXT,
                date TEXT,
                folderName TEXT,
                javaHistory TEXT,
                naviString TEXT,
                naviCode TEXT,
                org TEXT,
                partialToken TEXT,
                toolName TEXT,
                toolCode TEXT, 
                projectType TEXT,
                paired BOOLEAN,
                regression BOOLEAN
)"
      dbExecute(con, query)
    }

    # Check if a row with the matching folderName already exists
    check_query <- sprintf("SELECT COUNT(*) FROM project WHERE folderName='%s' AND projectType='project'", folderName)
    existing_count <- dbGetQuery(con, check_query)[[1]]

    if (existing_count > 0) {
      # Update existing row
      update_query <- sprintf("UPDATE project SET userId='%s', name='%s', description='%s', module='%s', moduleReadable='%s', dataType='%s',
                 date='%s', javaHistory='%s', naviString='%s', naviCode='%s', org='%s', partialToken='%s',
                 toolName='%s', toolCode='%s', projectType='%s', paired=%s, regression=%s WHERE folderName='%s'",
                 uid, name, description, module, moduleReadable, dataType, date, javaHistory, naviString, currentNaviCode,
                 org, partialToken, toolName, toolCode, projectType, paired, regression, folderName)
      dbExecute(con, update_query)
      result <- 2  # Successfully updated
    } else {
      # Insert new row
      insert_query <- "INSERT INTO project (userId, name, description, module, moduleReadable, dataType,
                 date, folderName, javaHistory, naviString, naviCode, org, partialToken,
                 toolName, toolCode, projectType, paired, regression) VALUES
                 ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      dbExecute(con, insert_query, list(uid, name, description, module, moduleReadable, dataType, date, folderName,
                               javaHistory, naviString, currentNaviCode, org, partialToken, toolName, toolCode, projectType, paired, regression))
      result <- 1  # Successfully inserted
    }
    
    dbCommit(con)
    
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbRollback(con)  # Roll back the transaction in case of an error
    }
    print(paste("An error occurred:", e$message))
  }, finally = {
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)  # Disconnect if connection was actually established
    }
  })
  
  return(result)
}

UpdateProjectTitleDescription <- function(newName, newDescription,id) {
  
  # Load necessary libraries
  library(RSQLite)
  library(DBI)
  
  result <- 0
  
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
    
    dbBegin(con)
    
    # Check if table exists, if not return an error
    if(!"project" %in% dbListTables(con)) {
      stop("Table 'project' does not exist in the database.")
    }
    
    # Check if a row with the given id exists
    query <- "SELECT id FROM project WHERE id = ?"
    existingRow <- dbGetQuery(con, query, list(id))
    
    if(nrow(existingRow) > 0) {
      # Update the existing row
      query <- "UPDATE project SET name=?, description=? WHERE id=?"
      dbExecute(con, query, list(newName, newDescription, id))
      result <- 1  # Successfully updated
    } else {
      print("No row exists with the given id.")
    }
    
    dbCommit(con)
    
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbRollback(con)  # Roll back the transaction in case of an error
      print(paste("An error occurred:", e$message))
    }
  }, finally = {
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)  # Disconnect if connection was actually established
    }
  })
  
  return(result)
}


getProjectsFromSQLite <- function(email, toolName = "MetaboAnalyst") {
  library(RSQLite)
  library(DBI)
  con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
  
  transactionStarted <- FALSE
  
  on.exit({
    if(transactionStarted) {
      dbRollback(con)
    }
    dbDisconnect(con)
  }, add = TRUE)
  
  tryCatch({
    dbBegin(con)
    transactionStarted <- TRUE
    
    query <- "SELECT * FROM project WHERE userId = ? AND toolName = ? AND projectType = 'project'"
    res <- dbGetQuery(con, query, params = list(email, toolName))
    res <- res[order(-res$id), ];
    if (nrow(res) == 0) {
      return(NULL)  # Return NULL if no matches
    }
    dbCommit(con)
    transactionStarted <- FALSE
  }, error = function(e) {
    stop("An error occurred while fetching data: ", e)
  })
  

  # Convert the data frame to a list of lists
  res_list <- split(res, seq(nrow(res)))
  res_list <- lapply(res_list, function(x) as.list(x))

    # Optional: If you want to make sure each element in the lists is a scalar, you can add this
    res_list <- lapply(res_list, function(single_row_list) {
      lapply(single_row_list, function(x) {
        if (is.vector(x) && length(x) == 1) {
          x <- x[[1]]
        }
        return(x)
      })
    })

  return(res_list)
}

DeleteProjectById <- function(id) {
  # Load necessary libraries
  library(RSQLite)
  library(DBI)
  
  # Connect to SQLite database
  con <- dbConnect(RSQLite::SQLite(), projectDbPath)
  
  result <- 0  # Initialize result status
  
  tryCatch({
    # Begin a transaction
    dbBegin(con)
    
    # SQL query to delete the entry by id
    query <- sprintf("DELETE FROM project WHERE id = %d", id)
    
    # Execute the delete query
    dbExecute(con, query)
    
    # Commit the transaction
    dbCommit(con)
    
    result <- 1  # Successfully deleted
  }, error = function(e) {
    # Rollback if an error occurred
    dbRollback(con)
  }, finally = {
    # Disconnect from the database
    dbDisconnect(con)
  })
  
  return(result)
}

loadProject <- function(token) {
  library(RSQLite)
  library(DBI)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
  transactionStarted <- FALSE  # Add a flag to track if the transaction is active

  on.exit({
    if (transactionStarted) {  # Only rollback if transaction was actually started
      dbRollback(con)
    }
    dbDisconnect(con)
  }, add = TRUE)
  
  tryCatch({
    dbBegin(con)
    transactionStarted <- TRUE  # Set flag to TRUE as transaction is now active
    
    query <- sprintf("SELECT * FROM project WHERE partialToken='%s'", token)
    res <- dbGetQuery(con, query)
    
    if(nrow(res) == 0) {
      dbRollback(con)
      return(NULL)
    }

    dbCommit(con)
    transactionStarted <- FALSE  # Set flag to FALSE as transaction has been committed
    return(res)
  }, error = function(e) {
    if (transactionStarted) {
      dbRollback(con)
    }
    stop("An error occurred while fetching data: ", e)
  })
}


##
##EMAIL
##


InsertToken <- function(userID, resetToken, ExpDate) {
  library(RSQLite)
  library(DBI)
  
  # Initialize the connection to NULL
  con <- NULL
  
  # Wrap the main logic in a tryCatch for error handling
  result <- tryCatch({
    # Establish SQLite connection
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
    
    # Check if the 'tokens' table exists
    tables <- DBI::dbListTables(con)
    if (!"tokens" %in% tables) {
      # If not, create the 'tokens' table
      create_table_query <- "
        CREATE TABLE tokens (
          userNM INTEGER,
          tokens TEXT,
          ExpDate TEXT
        )"
      DBI::dbExecute(con, create_table_query)
    }
    
    # Formulate the query using parameterized binding for safety
    query <- "INSERT INTO tokens (userNM, tokens, ExpDate) VALUES (?, ?, ?)"
    
    # Bind parameters and execute the query
    DBI::dbExecute(con, query, params = list(userID, resetToken, as.character(ExpDate)))
    
    # Return a success message
    return("Insert successful.")
  }, 
  error = function(err) {
    # Return the error message as a string
    return(sprintf("Error encountered: %s", conditionMessage(err)))
  },
  finally = {
    # Ensure the connection is always closed
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
  
  return(result)
}

CheckUserExists <- function(email) {
  library(RSQLite)
  library(DBI)

  # Initialize the connection to NULL
  con <- NULL

  # Wrap the main logic in a tryCatch for error handling
  result <- tryCatch({
    # Establish SQLite connection
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)

    # Formulate the query
    query <- sprintf("SELECT COUNT(*) as count FROM users WHERE email = '%s'", email)

    # Execute the query
    res <- DBI::dbGetQuery(con, query)
    
    # Return the result as a string
    if (res$count > 0) {
      return("User exists.")
    } else {
      return("User does not exist.")
    }
  }, 
  error = function(err) {
    # Return the error message as a string
    return(sprintf("Error encountered: %s", conditionMessage(err)))
  },
  finally = {
    # Ensure the connection is always closed
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })

  return(result)
}

VerifyToken <- function(token) {
  library(RSQLite)
  library(DBI)

  # Initialize the connection to NULL
  con <- NULL

  # Wrap the main logic in a tryCatch for error handling
  result <- tryCatch({
    # Establish SQLite connection
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)

    # Formulate the query using parameterized binding for safety
    query <- "SELECT userNM, expDate FROM tokens WHERE tokens = ?"

    # Fetch the data using the parameterized query
    data <- DBI::dbGetQuery(con, query, params = list(token))
    
    # Check if data was fetched and return appropriate message
    if (nrow(data) > 0) {
      return(data$userNM[1])
    } else {
      return("Error encountered: Token not found.")
    }
  },
  error = function(err) {
    # Return the error message as a string
    return(sprintf("Error encountered: %s", conditionMessage(err)))
  },
  finally = {
    # Ensure the connection is always closed
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })

  return(result)
}

DeleteTokenForUser <- function(email) {
  library(RSQLite)
  library(DBI)

  # Initialize the connection to NULL
  con <- NULL

  # Wrap the main logic in a tryCatch for error handling
  result <- tryCatch({
    # Establish SQLite connection
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)

    # Formulate the query using parameterized binding for safety
    query <- "DELETE FROM tokens WHERE userNM = ?"

    # Execute the deletion query
    DBI::dbExecute(con, query, params = list(email))

    # Return a success message
    return("Success")
  },
  error = function(err) {
    # Return the error message as a string
    return(sprintf("Error encountered: %s", conditionMessage(err)))
  },
  finally = {
    # Ensure the connection is always closed
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })

  return(result)
}

ResetPassword <- function(new_password, email) {
  library(RSQLite)
  library(DBI)

  # You'll also need to load the 'digest' package for hashing the password
  library(digest)

  # Initialize the connection to NULL
  con <- NULL

  result <- tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
    dbBegin(con)  # Start a transaction

    # Hash the new password
    hashed_password <- digest::digest(new_password, algo = "sha256")

    # Check if the email exists
    query <- sprintf("SELECT * FROM users WHERE email = '%s'", email)
    user_data <- dbGetQuery(con, query)

    if (nrow(user_data) == 0) {
      dbRollback(con)  # Roll back the transaction
      return("Email not found.")
    }

    # Update the password for the provided email
    update_query <- sprintf("UPDATE users SET password = '%s' WHERE email = '%s'", hashed_password, email)
    DBI::dbExecute(con, update_query)

    dbCommit(con)  # Commit the transaction
    return("Success")
  },
  error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbRollback(con)  # Roll back the transaction in case of an error
      dbDisconnect(con)  # Disconnect if connection was actually established
    }
    return(paste0("Reset Error - ", e$message))
  })

  return(result)
}

AddActivationCode <- function(activateCode, expDate, email) {
  library(DBI)
  library(RSQLite)

  # Initialize the connection to NULL
  con <- NULL

  # Wrap the main logic in a tryCatch for error handling
  result <- tryCatch({
    # Establish SQLite connection
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = projectDbPath)

    # Create the SQL update query
    query <- paste("UPDATE users SET activateCode = '", activateCode,
                   "', expDate = '", expDate,
                   "' WHERE email = '", email, "';", sep = "")

    # Execute the update query
    DBI::dbExecute(con, query)

    # Return a success message
    return("Update successful.")
  },
  error = function(err) {
    # Return the error message as a string
    return(sprintf("Error encountered: %s", conditionMessage(err)))
  },
  finally = {
    # Ensure the connection is always closed
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })

  return(result)
}

CheckActivationCode <- function(email, activationCode) {
  library(DBI)
  library(RSQLite)

  # Initialize the connection to NULL
  con <- NULL

  # Initialize a variable to store the query result
  query_result <- NULL

  # Wrap the main logic in a tryCatch for error handling
  result <- tryCatch({
    # Establish SQLite connection
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = projectDbPath)

    # Formulate the SQL SELECT query with parameterized binding
    query <- "SELECT email, activateCode, expDate FROM users WHERE email = ?"

    # Execute the query with parameter binding
    query_result <- DBI::dbGetQuery(con, query, params = list(email))

    # Check if a matching user was found
    if (nrow(query_result) == 0) {
      return("No user found with the provided email.")
    }

    # Check if the activation code matches
    if (query_result$activateCode != activationCode) {
      return("Wrong activation code.")
    }

    # Check if ExpDate is within one hour of the current time
    expDate <- as.POSIXct(query_result$expDate, format = "%Y-%m-%d %H:%M:%S")
    currentDateTime <- Sys.time()
    
    if (expDate < currentDateTime) {
      return("Activation code has expired.")
    }

    # If all checks pass, update the 'activated' column to 1
    DBI::dbExecute(con, "UPDATE users SET activated = 1 WHERE email = ?", params = list(email))

    # Return a success message
    return("Success!")

  },
  error = function(err) {
    # Return the error message as a string
    return(sprintf("Error encountered: %s", conditionMessage(err)))
  },
  finally = {
    # Ensure the connection is always closed
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })

  return(result)
}


DeleteUserAndProjects <- function(userId) {
  library(RSQLite)
  library(DBI)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
  transactionStarted <- FALSE

  on.exit({
    if (transactionStarted) {
      dbRollback(con)
    }
    dbDisconnect(con)
  }, add = TRUE)
  
  tryCatch({
    # Begin transaction
    dbBegin(con)
    transactionStarted <- TRUE

    # Delete associated projects
    delete_projects_query <- sprintf("DELETE FROM project WHERE userId='%s'", userId)
    dbExecute(con, delete_projects_query)

    # Delete the user
    delete_user_query <- sprintf("DELETE FROM users WHERE email='%s'", userId)
    dbExecute(con, delete_user_query)

    # Commit transaction
    dbCommit(con)
    transactionStarted <- FALSE

    return("Successfully deleted user and associated projects.")
  }, error = function(e) {
    if (transactionStarted) {
      dbRollback(con)
    }
    return("Error while deleting user and projects: ", e$message)
  })
}

CheckMatchingFolderNameProject <- function(folderName) {
  library(RSQLite)
  library(DBI)
  
  con <- dbConnect(RSQLite::SQLite(), projectDbPath)
  resultMessage <- ""
  matchedId <- -1  # Initialize variable to hold the matched id, if any

  on.exit({
    dbDisconnect(con)
  }, add = TRUE)
  
  tryCatch({
    # Query to check for a matching folderName and projectType
    check_query <- sprintf("SELECT id FROM project WHERE folderName='%s' AND projectType='project'", folderName)
    result <- dbGetQuery(con, check_query)
    
    if (nrow(result) > 0) {
      matchedId <- result$id[1]  # Assuming the 'id' column holds the IDs
      resultMessage <- sprintf("Matching row found with id %d.", matchedId)
    } else {
      resultMessage <- "No matching row found."
    }

  }, error = function(e) {
    resultMessage <- paste("Error while checking for matching row: ", e$message)
  })

  return(matchedId)
}

HashPassword <- function(raw_password){
    hashed_password <- digest::digest(raw_password, algo = "sha256");
    hashed_password;
}