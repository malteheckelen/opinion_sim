### Interface

# Example for SpaDES

"
mySim <- simInit(
 times = list(start = 0.0, end = 2.0, timeunit = "year"),
 params = list(
   .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
 ),
 modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
 paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
)
spades(mySim, .plotInitialTime = NA)
"


# FUNCTION FOR PARAMETER SELECTION THAT INCLUDES simInit()

# FUNCTION FOR SETTING EXPERIMENT RUN PARAMETERS

# FUNCTION FOR EXECUTION



# - options=save output, plot etc


set_parameters <- function(times, modules) {
  
  simInit(
    
    times = list(
      
    )
    
    modules = list(
      
    )
    
    params = list(
      
    )
    
    paths = List(
      modulePath="./modules"
    )
    
  )
  
}

