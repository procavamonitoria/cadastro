# polished::deploy_app(
#   app_name = "semea",
#   app_dir = "C:/Users/BZunguze/OneDrive - ACQUABUILD/APPS 2022/BASELINE/BASELINE_SHINY",
#   api_key = "SWy1B0tMYmqPTk9BnojCMw3o5leplM4x8L",
#   launch_browser = TRUE,
#   r_ver = NULL,
#   cache = TRUE,
#   gh_pat = NULL,
#   max_sessions = Inf
# )

library(polished)
deploy_app("semea",
           app_dir = ".", 
           api_key = "SWy1B0tMYmqPTk9BnojCMw3o5leplM4x8L",
           launch_browser = TRUE, 
           region = "us-east1", 
           ram_gb = 2, 
           r_ver = NULL,
           tlmgr = character(0), 
           golem_package_name = NULL,
           cache = TRUE, 
           gh_pat = NULL,
           max_sessions = Inf)