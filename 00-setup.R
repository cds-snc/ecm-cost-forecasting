install.packages("renv")

renv::restore()  # Load the package snapshot

# Store the username in your keyring
invisible({
  tryCatch(
    { keyring::key_get("ECM-DB-WRITE-USER") },
    error = function(e) { 
      keyring::key_set("ECM-DB-WRITE-USER", 
                       prompt = "Enter username for database write user") 
    }
  )
})

# Store the password in your keyring
invisible({
  tryCatch(
    { keyring::key_get("ECM-DB-WRITE-PASS") },
    error = function(e) { 
      keyring::key_set("ECM-DB-WRITE-PASS", 
                       prompt = paste("Enter the password for user", keyring::key_get("ECM-DB-WRITE-USER"))) 
    }
  )
})

