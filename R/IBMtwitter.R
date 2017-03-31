# Script to connect the IBM Twitter API
# and search for the result

IBMtwitter <- function(username, password, api_server = "cdeservice.mybluemix.net"){



  if(username == "" | password == ""){
    stop("Login information is missing")
  }

  current_env <- environment()


  methods = list(
    current_env = current_env,
    search = function(keywords, count=100){
      if(is.na(keywords) | keywords == ""){
        stop("Search keyword is missing")
      }else{
        if(class(count) != "numeric"){
          count <- floor(as.numeric(count))
        }
        if(is.na(count)){
          stop("Invalid number of records")
        }else{
          if(count > 500){
            count <- 500
            warning("Maximun value for number or records is 500")
          }

          unikeyword <- utils::URLencode(keywords)

          username = get("username", current_env)
          password = get("password", current_env)
          api_server = get("api_server", current_env)

          ibm_call <- RCurl::getURL(paste0('https://',username,':',
                                    password,
                                    '@',api_server,
                                    '/api/v1/messages/search?q=', unikeyword,
                                    "&size=", count))

          if(!jsonlite::validate(ibm_call)){
            stop("Not a valid json format")
          }

          result_json <- jsonlite::fromJSON(ibm_call)

          assign("json", ibm_call, current_env)

          final_data <- data.frame(author.gender=result_json$tweets$cde$author$gender,
                                   author.location.country=result_json$tweets$cde$author$location$country,
                                   author.location.city=result_json$tweets$cde$author$location$city,
                                   author.location.state=result_json$tweets$cde$author$location$state,
                                   author.isparent=result_json$tweets$cde$author$parenthood$isParent,
                                   author.married=result_json$tweets$cde$author$maritalStatus$isMarried,
                                   content.sentiment.evidence=this$list2str(result_json$tweets$cde$content$sentiment$evidence),
                                   content.sentiment.polarity=result_json$tweets$cde$content$sentiment$polarity,
                                   message.time=result_json$tweets$message$postedTime,
                                   message.link=result_json$tweets$message$link,
                                   message.verb=result_json$tweets$message$verb,
                                   message.body=result_json$tweets$message$body,
                                   message.generator=result_json$tweets$message$generator$displayName,
                                   message.generator.link=result_json$tweets$message$generator$link,
                                   message.favourites.count=result_json$tweets$message$favoritesCount,
                                   message.objecttype=result_json$tweets$message$objectType,
                                   message.twitter_filter_level=result_json$tweets$message$twitter_filter_level,
                                   message.twitter_lang=result_json$tweets$message$twitter_lang,
                                   message.id=result_json$tweets$message$id,
                                   message.retweet_count=result_json$tweets$message$retweetCount,
                                   author.summary=result_json$tweets$message$actor$summary,
                                   author.image=result_json$tweets$message$actor$image,
                                   author.status_count=result_json$tweets$message$actor$statusesCount,
                                   author.utc_offset=result_json$tweets$message$actor$utcOffset,
                                   author.languages=this$list2str(result_json$tweets$message$actor$languages),
                                   author.preferred_username=result_json$tweets$message$actor$preferredUsername,
                                   author.display_name=result_json$tweets$message$actor$displayName,
                                   author.post_time=result_json$tweets$message$actor$postedTime,
                                   author.link=result_json$tweets$message$actor$link,
                                   author.veridied=result_json$tweets$message$actor$verified,
                                   author.friends_count=result_json$tweets$message$actor$friendsCount,
                                   author.twitter_time_zone=result_json$tweets$message$actor$twitterTimeZone,
                                   author.favourites_count=result_json$tweets$message$actor$favoritesCount,
                                   author.listed_count=result_json$tweets$message$actor$listedCount,
                                   author.objecttype=result_json$tweets$message$actor$objectType,
                                   author.id=result_json$tweets$message$actor$id,
                                   author.followers_count=result_json$tweets$message$actor$followersCount,
                                   author.links=this$list2str(result_json$tweets$message$actor$links),
                                   author.location=result_json$tweets$message$actor$location$displayName,
                                   author.location.type=result_json$tweets$message$actor$location$objectType,
                                   provider=result_json$tweets$message$provider$displayName,
                                   provider.link=result_json$tweets$message$provider$link,
                                   message.ent_urls=this$list2str(result_json$tweets$message$twitter_entities$urls),
                                   message.hashtags=this$list2str(result_json$tweets$message$twitter_entities$hashtags),
                                   message.user_mentions=this$list2str(result_json$tweets$message$twitter_entities$user_mentions),
                                   message.trends=this$list2str(result_json$tweets$message$twitter_entities$trends),
                                   message.symbols=this$list2str(result_json$tweets$message$twitter_entities$symbols),
                                   message.media=this$list2str(result_json$tweets$message$twitter_entities$media),
                                   message.ext_media=this$list2str(result_json$tweets$message$twitter_extended_entities$media),
                                   message.location.country_code=result_json$tweets$message$location$country_code,
                                   message.location.display_name=result_json$tweets$message$location$displayName,
                                   message.location.name=result_json$tweets$message$location$name,
                                   message.location.twitter_country_code=result_json$tweets$message$location$twitter_country_code,
                                   message.location.link=result_json$tweets$message$location$link,
                                   gnip.language=result_json$tweets$message$gnip$language$value,
                                   gnip.location=this$profileLocations2df(result_json$tweets$message$gnip$profileLocations)
          )

          return(final_data)

        }
      }
    },
    list2str = function(x){
      if(length(x) == 0){
        return(NA)
      }else{
        temp_result <- sapply(x, function(y){
          if(length(y) == 0){
            return("")
          }else{
            return(paste(as.character(y), collapse = " "))
          }
        })
        return(temp_result)
      }
    },
    profileLocations2df = function(df){
      temp_file <- lapply(df,function(x){
        if(!is.null(x)){
          x_cod <- fill_empty(x$geo$coordinates[[1]][1])
          y_cod <- fill_empty(x$geo$coordinates[[1]][1])
          country <- fill_empty(x$address$country)
          country_code <- fill_empty(x$address$countryCode)
          locality <- fill_empty(x$address$locality)
          region <- fill_empty(x$address$region)
          geo_type <- fill_empty(x$geo$type)

          return(data.frame(lat=x_cod, lon=y_cod, country=country,
                            country_code=country_code, locality=locality,
                            region=region, geo_type=geo_type))
        }else{
          return(data.frame(lat=NA, lon=NA, country=NA,
                            country_code=NA, locality=NA,
                            region=NA, geo_type=NA))
        }
      })
      return(do.call(rbind, temp_file))
    }

  )
  assign('this', methods, envir = current_env)
  class(methods) <- append(class(methods), "IBMtwitter")
  return(methods)
}

fill_empty <- function(value){
  if(is.null(value)){
    return(NA)
  }else{
    return(value)
  }
}

