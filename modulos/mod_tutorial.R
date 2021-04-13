
mod_tutorial_ui <- function(id){
    tagList(
        tags$iframe(
            src = 'tutorial/tutorial.html',
            width = '100%', height = '13100px', 
            frameborder = 0, scrolling = 'no'
        )
    )
}
