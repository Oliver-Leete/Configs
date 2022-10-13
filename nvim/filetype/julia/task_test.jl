path_to_test = ARGS[1]
package_name_to_run = basename(path_to_test)

using Pkg

Pkg.test(package_name_to_run, coverage=true)

empty!(Base.LOAD_PATH)
push!(Base.LOAD_PATH, joinpath(@__DIR__, "..", "packages"))
push!(Base.LOAD_PATH, "@stdlib")

import CoverageTools

coverage = CoverageTools.process_folder()

CoverageTools.LCOV.writefile("lcov.info", coverage)

CoverageTools.clean_folder(path_to_test)
