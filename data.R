# load libraries
library(buffer)
library(dplyr)
library(stringr)

# function to collect messages and tags
get_messages <- function() {
  
  # connect to redshift
  con <- redshift_connect()
  
  # define the query
  message_query <- "
    select 
      m.id as message_id
      , m.conversation_id
      , m.created_at
      , m.created_by_first_name
      , m.created_by_last_name
      , m.was_created_by_customer
      , c.mailbox_name
      , c.tags
      , c.created_by_type
      , c.subject
      , c.preview
      , f.created_at as custom_field_created_at
      , f.name as custom_field_type
      , f.option_label as custom_field_label
      , m.body
      , rank() over (partition by m.conversation_id order by m.created_at) as message_rank
      , rank() over (partition by m.id order by f.created_at) as custom_field_rank
    from dbt.helpscout_messages as m
    inner join dbt.helpscout_conversations as c
    on c.id = m.conversation_id
    and m.was_created_by_customer
    left join dbt.helpscout_custom_fields as f
    on c.id = f.conversation_id
    where custom_field_label is not null
    and created_by_type = 'customer'
    group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
  "
  
  # query redshift
  messages <- query_db(message_query, con)
  
  # return messages with tags
  messages
}


# function to clean the data
clean_data <- function(df) {
  
  # set rank as integer
  df$message_rank <- as.integer(df$message_rank)
  
  # look only at area tags for now
  df <- filter(df, custom_field_type == 'AREA')
  
  # filter to only include first message
  df <- filter(df, message_rank == 1)
  
  # only include first custom field label for now
  df <- filter(df, custom_field_rank == 1)
  
  # get full name
  df$full_name <- paste(df$created_by_first_name, df$created_by_last_name)
  
  # merge uncategorized
  df$custom_field_label <- gsub("Uncategorized", "uncategorized", df$custom_field_label)
  
  # merge feature requests
  df$custom_field_label <- gsub("feature requests", "feature request", df$custom_field_label)
  
  # filter out certain labels
  df <- df %>% 
    filter(custom_field_label != "success" & custom_field_label != "extension - multiple composer")
  
  # remove emails from bufferbot
  df <- filter(df, full_name != "Updates Buffer")
  
  # remove html df and line breaks from body
  df <- df %>% 
    mutate(text = gsub("<.*?>", "", body),
           text = gsub("\\n", " ", text))
  
  # return dataframe
  df
}

