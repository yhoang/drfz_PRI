#' Login to community or premium cytobank server
#' 
#' @rdname logIn
#' @aliases logIn
#'
#' @details \code{logIn returns a session object} .
#' @examples \donttest{# Login to community server
#' community="https://community.cytobank.org/cytobank"
#' session=logIn(community,username,password)
#' }
#' @export
#'  
logIn=function(site,usr,pwd){
  session=rvest::html_session(paste0(site),"/login")            # create session
  form=rvest::html_form(session)[[3]]                           # pull form from session
  submit_button=list(name = NULL,                               # create submit button
                     type = "submit",                           # because new_user_session
                     value = NULL,                              # form has no submit button
                     checked = NULL,
                     disabled = NULL,
                     readonly = NULL,
                     required = FALSE)
  attr(submit_button,"class")="input"                           # set class attribute
  form[["fields"]][["submit"]]=submit_button                    # add button to form
  filled_form=rvest::set_values(form,`user_session[login]`=usr, # fill out form with
                                `user_session[password]`=pwd)   # username & password
  sendInfo=rvest::submit_form(session,filled_form)              # submit form to server
}