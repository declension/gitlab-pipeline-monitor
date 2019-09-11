module Config exposing (httpTimeout, maxBuildsPerBranch, maxNonDefaultBranches, maxPipelinesPerProject, maxProjects)


maxNonDefaultBranches =
    3


maxPipelinesPerProject =
    36


maxBuildsPerBranch =
    4


maxProjects =
    15


httpTimeout =
    10.0
