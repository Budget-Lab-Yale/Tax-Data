# The modules the pipeline's Python and R stages run under on Bouchet (run.py runs these lines before each command).
export REBUILD_PY_ENV='module purge; module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a'
export REBUILD_R_ENV='module purge; module load R/4.4.1-foss-2022b'
export REBUILD_R_LIBS=$HOME/R/4.4.1-foss-2022b
