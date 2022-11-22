using LinearAlgebra
using Distributions
using CSV, DataFrames
using DelimitedFiles
using Random
using JLD2

## Read in Choleskey Matrix and VAR shocks
P = Matrix{Float64}(CSV.read("./data/VAR_data/extension_addEPU/P.csv", DataFrame))
u = Matrix{Float64}(CSV.read("./data/VAR_data/extension_addEPU/u.csv", DataFrame))

## Rotation Function
rotate = function (X)
    A = rand(Normal(), dim(X), dim(X))

    Q = qr(A).Q
    R = qr(A).R

    ## Make sure the diagonal of R is all positive
    # R2 = diagm(sign.(diag(R))) * R
    Q2 = Q * diagm(sign.(diag(R)))

    ## Rotate the choleskey Matrix
    Ã = X * transpose(Q2)

    return Ã
end

@time rotate(P)

P_new = rotate(P)
u_new = u * inv(P_new)

check_signs = function (X)
    sigma = abs.(diagm(diag(X)))
    A = X * inv(sigma)

    signs = [
        A[1,1] >  0,
        A[2,1] >  0,
        A[3,1] >  0,
        A[4,1] >  0,
        # A[5,1] >  0,

        A[1,2] >  0,
        A[2,2] >  0,
        A[3,2] >  0,
        A[4,2] <=  0,
        # A[5,2] >  0,

        A[1,3] <= 0,
        A[2,3] <= 0,
        A[3,3] <= 0,
        A[4,3] <= 0,
        A[5,3] >  0,

        A[1,4] >  0,
        A[2,4] >  0,
        A[3,4] >  0,
        A[4,4] <= 0,
        A[5,4] >  0,

        A[1,5] >  0,
        # A[2,5] !=  0,
        A[3,5] <=  0,
        A[4,5] <= 0,
        A[5,5] >  0,

        abs(A[1,1]) > abs(A[3,1]),
        abs(A[2,1]) > abs(A[3,1]),

        abs(A[1,2]) > abs(A[2,2]),
        abs(A[2,2]) > abs(A[3,2]),

        abs(A[1,3]) < abs(A[2,3]),
        abs(A[2,3]) < abs(A[3,3]),

        abs(A[1,4]) < abs(A[2,4]),
        abs(A[2,4]) < abs(A[3,4]),

        X[1,1]^2 + X[1,2]^2 > X[1,3]^2 + X[1,4]^2,
        X[3,1]^2 + X[3,2]^2 < X[3,3]^2 + X[3,4]^2 
    ]
    
    return sum(signs) == length(signs)
   
end
check_shocks = function(s)

    shocks = [
        any(s[686:706,5] .> 3),  ## Black Monday
        any(s[5894:5914,5] .> 3),  ## Lehman
        sum(s[5706:6100,5] .> 3),  ## GFC
        any(s[6602:6644,5] .> 3),  ## DC
        any(s[8764:8785,5] .> 3),  ## CVD
        any(s[4150:4164,5] .> 3),  ## 9-11
    ]

    return sum(shocks) == length(shocks)
end


## Test matrix to show it's possible to get signs right...
A = [2 3 -1 1 1; 2 2 -2 2 0; 1 1 -3 3 -1; 1 -1 -1 -1 -1; 1 1 1 1 1]
check_signs(P)
check_signs(A)
check_shocks(u)

##
times = 10 # 20
iter = 10^6 
successes = []

one_iter = function ()
    for i in 1:iter
        P_new = rotate(P)
        if check_signs(P_new)
            #u_new = u * inv(P_new)
            #if check_shocks(u_new)
                append!(successes, [P_new])
            #end
        end
    end
end



## Rotate through a bunch of these!
Random.seed!(13805)

for t in 1:times
    @time one_iter()
    print(string(t) * ": " * string(length(successes)) * "\n")
    save_object("./data/VAR_data/extension_addEPU/success_rots.jld2", successes)
end

length(successes)

successes[1000]

## Write successes to CSVs to be read into R
for i in 1:length(successes)
    writedlm("./data/VAR_data/extension_addEPU/rotations/rot" * string(i) * ".csv" ,  successes[i], ',')
end

writedlm( "./data/VAR_data/extension_addEPU/rotations/test.csv",  successes[1], ',')
