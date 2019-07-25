module Config exposing (maxNonDefaultBranches, maxPipelinesPerProject, maxProjects, httpTimeout, maxBuildsPerBranch)


maxNonDefaultBranches =
    3


maxPipelinesPerProject =
    20

maxBuildsPerBranch = 5

maxProjects =
    20


httpTimeout =
    10.0
