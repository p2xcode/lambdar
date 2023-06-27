is_aws_cli_installed <- function() {
  which_aws <- Sys.which("aws")
  IS_AWS_INSTALLED <- ifelse(which_aws[[1]] == "", FALSE, TRUE)
  checkmate::assert_true(IS_AWS_INSTALLED)
}

is_docker_installed <- function() {
  which_docker <- Sys.which("docker")
  IS_DOCKER_INSTALLED <- ifelse(which_docker[[1]] == "", FALSE, TRUE)
  checkmate::assert_true(IS_DOCKER_INSTALLED)
}

#' check_system_deps
#' @noRd
check_system_dependencies <- function() {
  # logger::log_debug("[check_system_dependencies] Checking if `aws cli` is installed.")
  # is_aws_cli_installed()

  logger::log_debug("[check_system_dependencies] Checking if `docker` is installed.")
  is_docker_installed()

  logger::log_debug("[check_system_dependencies] Done.")
}

#' connect to an aws service
#' @param service character, the name of a service, e.g., "lambda" or "iam". Should
#' be a function exported by `{paws}` (see `getNamespaceExports("paws")`)
#' @examples
#' \dontrun{
#'   aws_connect("lambda")
#'   }
#' @export
aws_connect <- function(service) {
  logger::log_debug("[aws_connect] Checking requested service.")

  if (!service %in% getNamespaceExports("paws")) {
    msg <- glue::glue("The service `{service}` does not appear to be available in `paws`.")
    logger::log_error(msg)
    rlang::abort(msg)
  }

  logger::log_debug("[aws_connect] Connecting to AWS.")

  .service <- utils::getFromNamespace(service, "paws")
  .service()
}

#' install_cran_deps_line
#' @noRd
install_cran_deps_line <- function(cran_deps) {

  checkmate::assert_character(cran_deps)

  if (!is.null(cran_deps)) {
    logger::log_debug("[install_cran_deps_line] cran_deps exist.  Adding them to Dockerfile.")
    repo <- glue::single_quote('https://packagemanager.rstudio.com/all/__linux__/centos7/latest')
    cran_glued <- glue::glue_collapse(glue::single_quote(cran_deps), sep = ", ")
    glue::glue('RUN Rscript -e "install.packages(c({cran_glued}), repos = {repo})"')
  } else {
    logger::log_debug("[install_cran_deps_line] cran_deps do not exist.  Skipping...")
  }

}

#' install_guithub_deps_line
#' @noRd
install_github_deps_line <- function(github_deps) {

  checkmate::assert_character(github_deps)

  if (!is.null(github_deps)) {
    logger::log_debug("[install_github_deps_line] github_deps exist.  Adding them to Dockerfile.")
    github_glued <- glue::glue_collapse(glue::single_quote(github_deps), sep = ", ")
    glue::glue('RUN Rscript -e "remotes::install_github(c({github_glued}))"')
  } else {
    logger::log_debug("[install_github_deps_line] github_deps do not exist.  Skipping...")
  }


}

#' runtime_line
#' @noRd
runtime_line <- function(runtime) {
  checkmate::assert_character(runtime)
  rt <- glue::double_quote(runtime)
  glue::glue('CMD [{rt}]')
}

#' parse password from ecr token
#' @noRd
parse_password <- function(ecr_token) {
  ecr_token %>%
    jsonlite::base64_dec() %>%
    rawToChar() %>%
    gsub(pattern = "^AWS:", replacement = "", x = .)
}

#' create_lambda_dockerfile
#'
#' @param folder path to store the Dockerfile
#' @param runtime_function name of the runtime function
#' @param runtime_path path to the script containing the runtime function
#' @param cran_dependencies list of dependencies
#' @param github_dependencies list of dependencies
#'
#' @examples
#' \dontrun{
#'   runtime_function <- "parity"
#'   runtime_path <- system.file("parity.R", package = "r2lambda")
#'   folder <- "~/Desktop/parity-lambda"
#'   dependencies <- NULL
#'
#'   create_lambda_dockerfile(
#'     folder = folder,
#'     runtime_function = runtime_function,
#'     runtime_path = runtime_path,
#'     cran_dependencies = cran_dependencies,
#'     github_dependencies = github_dependencies
#'     )
#'   create_lambda_image(folder, tag = "test-tag")
#'   dir.exists(folder)
#'   dir(folder)
#'   unlink(folder, recursive = TRUE)
#' }
#' @noRd
create_lambda_dockerfile <-
  function(folder,
           runtime_function,
           runtime_path,
           cran_dependencies = NULL,
           force_rebuild_cran_deps = FALSE,
           github_dependencies = NULL,
           force_rebuild_github_deps = FALSE,
           overwrite = TRUE) {

    logger::log_debug("[create_lambda_dockerfile] Validating inputs.")

    checkmate::assert_character(
      x = folder,
      min.chars = 1,
      len = 1
    )

    if (checkmate::test_directory_exists(folder)) {
      if (overwrite) {
        msg <- glue::glue("[create_lambda_dockerfile] Directory {folder} exists. Using it.")
        logger::log_info(msg)


        # checking that files exist
        if (!checkmate::test_file_exists(paste(folder, "runtime.R", sep = "/"))) {
          msg <- glue::glue("[create_lambda_dockerfile] Can't access runtime script file {folder}. Bringing vanilla version from r2lambda package")
          #logger::log_error(msg)
          #rlang::abort(msg)
        }

        if (!checkmate::test_file_exists(paste(folder, "Dockerfile", sep = "/"))) {
          msg <- glue::glue("[create_lambda_dockerfile] Can't access Dockerfile {folder}.  Bringing vanilla version from r2lambda package")
          #logger::log_error(msg)
          #rlang::abort(msg)
        }
      } else {
        msg <- glue::glue("[create_lambda_dockerfile] Directory {folder} exists. Please choose another name.")
        logger::log_info(msg)
        rlang::abort(msg)
      }
    } else {
      dir.create(folder)
    }

    if (force_rebuild_cran_deps) system(paste("date >", file.path(folder, "marker_cran")))
    if (force_rebuild_github_deps) system(paste("date >", file.path(folder, "marker_github")))

    checkmate::assert_character(
      x = runtime_function,
      min.chars = 1,
      len = 1,
      any.missing = FALSE
    )

    checkmate::assert_character(
      x = runtime_path,
      min.chars = 1,
      len = 1,
      any.missing = FALSE
    )

    if (!checkmate::test_file_exists(runtime_path)) {
      msg <- glue::glue("[create_lambda_dockerfile] Can't access runtime script file {runtime_path}.")
      logger::log_error(msg)
      rlang::abort(msg)
    }

    checkmate::assert_character(
      x = cran_dependencies,
      min.chars = 1,
      null.ok = TRUE
    )

    checkmate::assert_character(
      x = github_dependencies,
      min.chars = 1,
      null.ok = TRUE
    )

    logger::log_debug("[create_lambda_dockerfile] Creating directory with Dockerfile and runtime script.")

    docker_template <- system.file("lambdr_dockerfile.txt", package = "r2lambda")

    dir.exists(folder)

    file.copy(runtime_path, folder)
    file.rename(from = file.path(folder, basename(runtime_path)),
                to = file.path(folder, "runtime.R"))

    file.copy(docker_template, folder, recursive = FALSE)
    file.rename(from = file.path(folder, basename(docker_template)),
                to = file.path(folder, "Dockerfile"))

    logger::log_debug("[create_lambda_dockerfile] Updating Dockerfile with dependencies and runtime info.")

    if (!is.null(cran_dependencies)) {
      if (force_rebuild_cran_deps) {
        write("COPY marker_cran /dev/null",
          file = file.path(folder, "Dockerfile"),
          append = TRUE)  
      }
      logger::log_debug("[create_lambda_dockerfile] cran_dependencies exist.  Adding them to Dockerfile.")  
      deps_string <- install_cran_deps_line(cran_deps = c(cran_dependencies))
      write(deps_string,
            file = file.path(folder, "Dockerfile"),
            append = TRUE)
    } else {
      logger::log_debug("[create_lambda_dockerfile] cran_dependencies do not exist.  Skipping...")  
    }

     if (!is.null(github_dependencies)) {
      if (force_rebuild_github_deps) {
        write("COPY marker_github /dev/null",
        file = file.path(folder, "Dockerfile"),
        append = TRUE)  
      }

      logger::log_debug("[create_lambda_dockerfile] github_dependencies exist.  Adding them to Dockerfile.")  
      deps_string <- install_github_deps_line(github_deps = c(github_dependencies))
      write(deps_string,
            file = file.path(folder, "Dockerfile"),
            append = TRUE)
    } else {
      logger::log_debug("[create_lambda_dockerfile] github_dependencies do not exist.  Skipping...")  
    }

    runtime_string <- runtime_line(runtime_function)

    write(runtime_string,
          file = file.path(folder, "Dockerfile"),
          append = TRUE)

    logger::log_debug("[create_lambda_dockerfile] Done.")
  }

#' Fetch or create ecr repo
#'
#' @param tag a tag for the image
#' @noRd
fetch_ecr_repo <- function(ecr_repo) {
  logger::log_debug("[fetch_ecr_repo] Checking if repository already exists.")
  ecr_service <- aws_connect("ecr")

  repos <- ecr_service$describe_repositories()$repositories
  repos_names <- sapply(repos, "[[", "repositoryName")

  if (ecr_repo %in% repos_names) {
    logger::log_debug("[fetch_ecr_repo] Repository exists. Fetching URI.")
    needed_repo <- repos_names == ecr_repo
    repo_uri <- repos[needed_repo][[1]]$repositoryUri
  } else {
    logger::log_debug("[fetch_ecr_repo] Creating new repository and fetching URI.")
    repo_meta <- ecr_service$create_repository(
      repositoryName = ecr_repo,
      imageScanningConfiguration = list(scanOnPush = TRUE)
    )
    repo_uri <- repo_meta$repository$repositoryUri
  }
  logger::log_debug("[fetch_ecr_repo] Done.")
  return(repo_uri)
}

#' create_lambda_container
#'
#' @param folder path to the folder containing the lambda runtime script and Dockerfile
#' @param ecr_repo the name of the erc repo tu push the image to
#' @param image_tag the tag to apply to the image in that repo
#' @noRd
create_lambda_image <- function(folder, ecr_repo, image_tag) {
  logger::log_debug("[create_lambda_image] Validating inputs.")
  checkmate::assert_character(ecr_repo)
  checkmate::assert_character(image_tag)
  checkmate::assert_character(folder)
  checkmate::assert_directory_exists(folder)

  checkmate::assert_file_exists(file.path(folder, "Dockerfile"))
  checkmate::assert_file_exists(file.path(folder, "runtime.R"))

  logger::log_debug("[create_lambda_image] Confirming that files are not empty.")
  checkmate::assert_numeric(
    x = file.size(file.path(folder, "Dockerfile")),
    lower = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    .var.name = "Check if file is empty."
  )

  checkmate::assert_numeric(
    x = file.size(file.path(folder, "runtime.R")),
    lower = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    .var.name = "Check if file is empty."
  )

  logger::log_debug("[create_lambda_image] Create or fetch a remote tag.")
  repo_uri <- fetch_ecr_repo(ecr_repo = ecr_repo)

  logger::log_debug("[create_lambda_image] Building docker image.")

  docker_cli <- stevedore::docker_client()
  docker_cli$image$build(context = folder, tag = paste(repo_uri, image_tag, sep = ":"))

  logger::log_debug("[create_lambda_image] Done.")
}

#' push_lambda_container to AWS ECR
#'
#' @param ecr_repo the tag of an existing local image
#' @param image_tag
#' @noRd
push_lambda_image <- function(ecr_repo, image_tag) {
  logger::log_debug("[push_lambda_image] Validating inputs.")
  checkmate::assert_character(ecr_repo)
  checkmate::assert_character(image_tag)

  logger::log_debug("[push_lambda_image] Authenticating Docker with AWS ECR.")

  ecr_service <- aws_connect("ecr")
  ecr_token <- ecr_service$get_authorization_token()
  ecr_password <- parse_password(ecr_token$authorizationData[[1]]$authorizationToken)
  repo_uri <- fetch_ecr_repo(ecr_repo)
  server_address <- repo_uri %>% gsub(pattern = "/.*$", replacement = "", x = .)

  docker_cli <- stevedore::docker_client()
  docker_cli$login(username = "AWS",
                   password = ecr_password,
                   serveraddress = server_address)

  logger::log_debug("[push_lambda_image] Pushing Docker image to AWS ECR.")

  tryCatch(
    expr = {
      docker_cli$image$push(name = paste(repo_uri, image_tag, sep = ":"))
    },
    error = function(e) {
      Sys.sleep(2)
      logger::log_debug("[push_lambda_image] Pushing Docker image to AWS ECR. Second try.")
      docker_cli$image$push(name = paste(repo_uri, image_tag, sep = ":"))
    }
  )

  logger::log_debug("[push_lambda_image] Done.")
  invisible(repo_uri)
}

#' create_lambda_exec_role
#' @param tag the tag of an existing local image
#' @noRd
create_lambda_exec_role <- function(tag) {

  logger::log_debug("[create_lambda_exec_role] Validating inputs.")
  checkmate::assert_character(tag)

  logger::log_debug("[create_lambda_exec_role] Getting Lambda role json.")
  role <- system.file("lambda-role.json", package = "r2lambda")
  role_string <- lambdr::as_stringified_json(jsonlite::fromJSON(role))
  role_name <- tag #paste(tag, ids::random_id(1,8), sep = "-")

  logger::log_debug("[create_lambda_exec_role] Checking if role already exists.")
  iam_service <- aws_connect("iam")

  role_meta <- tryCatch(
    expr = {
      role_meta <- iam_service$get_role(RoleName = role_name)
      logger::log_debug("[create_lambda_exec_role] role exists - reusing.")
      return(role_meta)
    },
    error = function(e) {
        logger::log_debug("[create_lambda_exec_role] role does not exist - creating a new role.")
    }
  )

  logger::log_debug("[create_lambda_exec_role] Creating Lambda execution role.")
  #iam_service <- aws_connect("iam")
  role_meta <- iam_service$create_role(
    RoleName = role_name,
    AssumeRolePolicyDocument = role_string
  )

  logger::log_debug("[create_lambda_exec_role] Attaching basic execution role policy.")
  iam_service$attach_role_policy(
    RoleName = role_name,
    PolicyArn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
    )

  logger::log_debug("[create_lambda_exec_role] Done.")
  invisible(role_meta)
}

#' delete_lambda_exec_role
#' @noRd
delete_lambda_exec_role <- function(tag) {
  ## TODO: this probably won't work as there is a policy attached to the role
  iam_service <- aws_connect("iam")
  iam_service$delete_role(
    RoleName = tag
  )
}

#' create_lambda_function
#'
#' @param tag the tag of an existing local image
#' @param ecr_image_uri the URI of the image to use
#' @param lambda_role_arn the arn of the execution role created for this lambda
#' @param set_aws_envvars logical, whether to set the local AWS secrets to the
#' deployed Lambda environment. This is useful if the Lambda needs to access
#' other AWS service. When `TRUE`, the following envvars are set: `PROFILE`, `REGION`,
#' `SECRET_ACCESS_KEY`, and `ACCESS_KEY_ID`. They are fetched using `Sys.getenv()`.
#'
#' @param ... arguments passed onto `paws.compute:::lambda_create_function()`.
#' For example `Timeout` to increase the execution time, and `MemorySize` to request
#' more memory
#'
#' @noRd
create_lambda_function <-
  function(ecr_repo, image_tag, runtime_function,
           ecr_image_uri,
           lambda_role_arn,
           set_aws_envvars = FALSE,
           ...) {

  logger::log_debug("[create_lambda_function] Validating inputs.")
  checkmate::assert_character(ecr_repo)
  checkmate::assert_character(image_tag)

  envvar_list <- list(Variables = list())

  ## TODO: enable adding arbitrary envvars
  if (isTRUE(set_aws_envvars)) {
    envvar_list <- list(
      Variables = list(
        REGION = Sys.getenv("REGION"),
        PROFILE = Sys.getenv("PROFILE"),
        SECRET_ACCESS_KEY = Sys.getenv("SECRET_ACCESS_KEY"),
        ACCESS_KEY_ID = Sys.getenv("ACCESS_KEY_ID")
      )
    )
  }

  lambda_service <- aws_connect("lambda")

  logger::log_debug("[create_lambda_function] Checking if function already exists")
  create_mode <<- ""

  lambda <- tryCatch(
    expr = {
      lambda_service$get_function(FunctionName = runtime_function)
      logger::log_debug("[create_lambda_function] Function already exists.  Updating function configuration...")
      create_mode <<- "update"
    },
    error = function(e) {
      logger::log_debug("[create_lambda_function] Function does not already exist.  Creating...")
      create_mode <<- "create"
    }
  )


  if (create_mode == "update") {
    #update
    lambda <- tryCatch(
      expr = {
        logger::log_debug("[create_lambda_function] Updating lambda function.")
        logger::log_debug("[create_lambda_function] Updating function configuration.")
        lambda_service$update_function_configuration(
          FunctionName = runtime_function,
          Role = lambda_role_arn,
          Environment = envvar_list,
          ImageConfig = list(EntryPoint=NULL, Command=runtime_function, WorkingDirectory=NULL),
          ...)

      last_update_status <- ""
      attempt <- 1
      while(last_update_status != "Successful" && attempt < 30) {
        Sys.sleep(2)
        last_update_status <- lambda_service$get_function(runtime_function)[["Configuration"]][["LastUpdateStatus"]]
        attempt <- attempt + 1        
      }

        logger::log_debug("[create_lambda_function] Updating function code.")
        lambda_service$update_function_code(
          FunctionName = runtime_function,
          ImageUri = glue::glue(paste("{ecr_image_uri}", image_tag, sep = ":")),
          ...)
      },
      error = function(e) {
        msg <- paste("[create_lambda_function] Failed to update function.", e$message)
        logger::log_error(msg)
        rlang::abort(msg)
      }
    )
  } else {
    #create
    lambda <- tryCatch(
      expr = {
        logger::log_debug("[create_lambda_function] Creating lambda function.")
        lambda_service$create_function(
          FunctionName = runtime_function,
          Code = list(ImageUri = glue::glue(paste("{ecr_image_uri}", image_tag, sep = ":"))),
          PackageType = "Image",
          Role = lambda_role_arn,
          Environment = envvar_list,
          ImageConfig = list(EntryPoint=NULL, Command=runtime_function, WorkingDirectory=NULL),
          ...)
      },
      error = function(e) {
        msg <- paste("[create_lambda_function] Failed to create function.", e$message)
        logger::log_error(msg)
        rlang::abort(msg)
      }
    )
  }

  logger::log_debug("[create_lambda_function] Done.")
  invisible(lambda)
}

#' delete_lambda_function
#' @noRd
delete_lambda_function <- function(tag) {
  lambda_service <- aws_connect("lambda")
  lambda_service$delete_function(
    FunctionName = tag
  )
}

#' Create a rule for event schedule
#'
#' @param rule_name character, the name of the rule (used later when adding targets to the event)
#' @param rate character, the rate at which we want the function to run, e.g., `5 minutes`, `1 minute`
#'
#' @noRd
create_event_rule_for_schedule <- function(rule_name, rate) {
  logger::log_debug("[create_event_rule_for_schedule] Creating rule for event schedule.")

  aws_event <- aws_connect("eventbridge")

  rule <- tryCatch(
    expr = {
      aws_event$put_rule(Name = rule_name,
                         ScheduleExpression = rate)
    },
    error = function(e) {
      logger::log_error(e$message)
      rlang::abort(e$message)
    }

  )

  logger::log_debug("[create_event_rule_for_schedule] Done.")
  return(rule$RuleArn)
}

#' Add permission for event to invoke lambda
#'
#' @param function_name character, the name of the lambda function we wish to schedule
#' @param scheduled_event_name character, a name for our event
#' @param scheduled_rule_arn character, the ARN of the scheduled event role (as
#' returned from `create_event_rule_for_schedule`)
#'
#' @noRd
lambda_add_permission_for_schedule <-
  function(function_name,
           scheduled_event_name,
           scheduled_rule_arn) {

    logger::log_debug("[lambda_add_permission_for_schedule] Adding permission for event schedule.")
    aws_lambda <- aws_connect("lambda")
    aws_lambda$add_permission(
      FunctionName = function_name,
      StatementId = scheduled_event_name,
      Action = "lambda:InvokeFunction",
      Principal = "events.amazonaws.com",
      SourceArn = scheduled_rule_arn
    )
    logger::log_debug("[lambda_add_permission_for_schedule] Done.")
  }

#' Add lambda to eventbridge rule
#'
#' @param rule_name character, the name of the rule
#' @param lambda_function_arn character, the ARN of the lambda function
#'
#' @noRd
add_lambda_to_eventridge <-
  function(rule_name, lambda_function_arn) {
    logger::log_debug("[add_lambda_to_eventridge] Adding lambda function to events.")
    aws_event <- aws_connect("eventbridge")
    aws_event$put_targets(Rule = rule_name, Targets = list(list(Id = 1, Arn = lambda_function_arn)))
    logger::log_debug("[add_lambda_to_eventridge] Done.")
  }


