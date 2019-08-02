module Config exposing (maxNonDefaultBranches, maxPipelinesPerProject, maxProjects, httpTimeout, maxBuildsPerBranch)


maxNonDefaultBranches =
    3


maxPipelinesPerProject =
    20

maxBuildsPerBranch = 4

maxProjects =
    16


httpTimeout =
    10.0
