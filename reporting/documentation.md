## Plan

#### General Outline

Have an interface, that flexibly loads further modules. These modules consist of 1.) the actual, raw simulation models that only contain the code for the actual agent interaction, 2.) the code for spatial implementations, 3.) the modules for restrictions of interaction processes (e.g. akin to Social Media filtering).

##### interface.R

Should contain code that a.) is an interface to ALL the other code and b.) loads modules and functions from these flexibly. A use case would be that a user wants to run a minimal bounded confidence model, but he wants to run it on a small world network instead of a lattice, and with the restriction of filtering. These all correspond to different implementations in the modules. While the main execution functions in interface.R should always have the same name, interface.R should source the different implementations depending on the user's choice.
