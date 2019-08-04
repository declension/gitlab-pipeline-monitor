module Config exposing (httpTimeout, maxBuildsPerBranch, maxNonDefaultBranches, maxPipelinesPerProject, maxProjects)


maxNonDefaultBranches =
    2


maxPipelinesPerProject =
    24


maxBuildsPerBranch =
    4


maxProjects =
    12


httpTimeout =
    10.0
