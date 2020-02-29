library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(openssl)
library(shinyAce)
library(markdown)



path_to = function(email, filetype){
  email_hash = sha256(email)
  # usr_data = read.csv(file = "www/usr/user_data.csv")
  if(filetype == "profile"){
    return(paste0("www/usr/", email_hash, "/profile.md"))
  }else if(filetype == "picture"){
    return(paste0("usr/", email_hash, "/pic.jpg"))
  }
}

ui = dashboardPagePlus(
  header = dashboardHeaderPlus(title = "Xbuddy", titleWidth = 300,
                               enable_rightsidebar = TRUE,
                               rightSidebarIcon = "cog"
  ),
  sidebar = dashboardSidebar(width = 300,
                             uiOutput("userpanel"),
                             sidebarMenu(
                               menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                               menuItem("Post Position", tabName = "Post", icon = icon("user-plus")),
                               menuItem("Profile", icon = icon("id-badge"),
                                        menuSubItem("View Profile", tabName = "View"),
                                        menuSubItem("Edit Profile", tabName = "Edit")
                               )
                             )

  ),
  body = dashboardBody(
    tabItems(
      tabItem("Dashboard",
              uiOutput("dashboard_output")
      ),
      tabItem("Post",
              uiOutput("post_output")
      ),
      tabItem("View",
              uiOutput("view_profile_output")
      ),
      tabItem("Edit",
              uiOutput("ace_editor"),
              
              actionButton("preview", "Preview Profile"),
              actionButton("save", "Save Profile"),
              hr(),
              box(title = "Profile Preview", footer = "Please press Save Profile to update Changes",
                  status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  htmlOutput("edit_profile_output")
              )
      )
    )
  ),
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(id = 1, icon = "sign-in-alt", active = TRUE,
                           actionBttn("login", "Log In", block = TRUE, style = "fill"),
                           actionBttn("logout", "Log Out", block = TRUE, style = "fill")
                           ),
    rightSidebarTabContent(id = 2, icon = "envelope-open-text", active = FALSE,
                           p("Here are the messages that you need to know")
    )

  )
)

server = function(input, output, session) {
  
  observeEvent(input$login, {
    showModal(modalDialog(
      tagList(
        textInput("email_input", NULL, placeholder = "Email Address"),
        passwordInput("password", NULL, placeholder = "Password")
      ), 
      title="Log In",
      footer = tagList(actionBttn("submit_login", "Log In", block = TRUE, style = "fill"),
                       actionBttn("goto_register", "Sign Up", block = TRUE, style = "fill", color = "success"),
                       actionBttn("cancel", "Cancel", block = TRUE, style = "fill", color = "royal"))
    ))
  })
  
  observeEvent(input$cancel, {
    removeModal()
  })
  
  observeEvent(input$goto_register, {
    showModal(modalDialog(
      tagList(
        textInput("first_name_input", NULL, placeholder = "First Name"),
        textInput("last_name_input", NULL, placeholder = "Last Name"),
        textInput("email_input", NULL, placeholder = "Email Address"),
        passwordInput("password", NULL, placeholder = "Password")
      ), 
      title="Log In",
      footer = tagList(actionBttn("register", "Register", block = TRUE, style = "fill"),
                       actionBttn("go_back_to_login", "Go Back to Log in", block = TRUE, 
                                  style = "fill", color = "success"))
    ))
  })
  
  observeEvent(input$go_back_to_login, {
    showModal(modalDialog(
      tagList(
        textInput("email_input", NULL, placeholder = "Email Address"),
        passwordInput("password", NULL, placeholder = "Password")
      ), 
      title="Log In",
      footer = tagList(actionBttn("submit_login", "Log In", block = TRUE, style = "fill"),
                       actionBttn("goto_register", "Sign Up", block = TRUE, style = "fill", color = "success"),
                       actionBttn("cancel", "Cancel", block = TRUE, style = "fill", color = "royal"))
    ))
  })
  
  output$userpanel = renderUI({
    sidebarUserPanel("Wanjun Gu",
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                     image = path_to(email = "wag001@ucsd.edu", filetype = "picture")
    )
  })
  
  output$view_profile_output = renderUI({
    box(width = 12,
        includeMarkdown(path_to(email = "jasonpanjialei@gmail.com", filetype = "profile"))
    )
  })
  
  output$ace_editor = renderUI({
    init = suppressWarnings(paste(readLines(path_to(email = "wag001@ucsd.edu", filetype = "profile"))))
    aceEditor("markdown_code", mode = "markdown", value = init)
  })
  
  output$edit_profile_output = renderUI({
    input$preview
      tryCatch({
        HTML(isolate(suppressWarnings(markdownToHTML(text = input$markdown_code,
                                    extensions = c("tables")))))
      }, error = function(e){
        # return("")
      })
  })
  
  onSessionEnded(function(){stopApp()})
}

shinyApp(ui, server)