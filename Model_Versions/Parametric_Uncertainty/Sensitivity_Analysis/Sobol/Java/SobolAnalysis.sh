#!/bin/bash

# Set up variables
PARAMS="params.txt"                 # parameters sampling bounds
SOBOLPAR="sobolParameterSets.txt"	# parameter sets generated from Sobol Sequence
MODEL_EXE="Sobol_vanDantzig.R"      # name of the executable
OUTPUT_FILE="sobolIndices"          # output file containing Sobol indices

INIT_SAMPLE=1000                    # number of initial samples to generate from Sobol Sequence
NUMOB=4                             # number of objectives
OBJS=$(seq 1 ${NUMOB})

# Generate parameter inputs using R Script [parameter name, lower bound, upper bound]
# If generating parameter inputs manually, comment out the following line line:
Rscript ./Parameters/params.R

# arguments for java input
JAVA_ARGS="-classpath MOEAFramework-2.0-Executable.jar"
JAVA_ARGS2="-Xmx256m -classpath MOEAFramework-2.0-Executable.jar"
SAMPLEGEN_ARGS="org.moeaframework.analysis.sensitivity.SampleGenerator"
SOBOL_ARGS="org.moeaframework.analysis.sensitivity.SobolAnalysis"

# GENERATION OF PARAMETER SETS USING SOBOL SEQUENCE
echo "Generating parameter sets using Sobol Sequence"
java ${JAVA_ARGS} ${SAMPLEGEN_ARGS} -m saltelli -n ${INIT_SAMPLE} -p Parameters/${PARAMS} > ${SOBOLPAR}

# RUN THE MODEL WITH THE PARAMETER SETS
echo "Running van Dantzig model..."
Rscript ${MODEL_EXE}

# COMPUTATION OF SOBOL INDICES FOR EACH OBJECTIVE
echo "Computation of Sobol Indices"

OBJ_NAME=("total_costs" "costs" "damages" "reliability") # Create vector of objective namess

for N in ${OBJS}
do
NAME=${OUTPUT_FILE}_${OBJ_NAME[N-1]}
let "OB=${N}-1"
java ${JAVA_ARGS2} ${SOBOL_ARGS} -m ${OB} -i objectiveValues.txt -p Parameters/${PARAMS} -o Output/${NAME}.txt
done

echo "Sobol Analysis complete"
