# Tax-Data
Produces historical and projected tax microdata files 

## Local Standalone Use

`Tax-Data` expects a dependency tree rooted at the paths configured in
`config/interfaces/output_roots.yaml`. For local or public standalone use, the
easiest option is to keep all inputs under a single local root and override the
default cluster paths at runtime with environment variables.

Supported overrides:

- `TAX_DATA_RUNSCRIPT_ID`: runscript name without `.yaml` suffix. Defaults to `baseline`.
- `TAX_DATA_WRITE_LOCALLY`: overrides `runtime_options.write_locally` from the runscript.
- `TAX_DATA_USER_ID`: overrides `runtime_options.user_id` from the runscript.
- `TAX_DATA_LOCAL_ROOT`: replaces `output_roots$local`.
- `TAX_DATA_PRODUCTION_ROOT`: replaces `output_roots$production`.
- `TAX_DATA_INPUT_ROOT`: optional explicit root for dependency reads. If unset,
  inputs are read from the active local/production root for the run.

Example:

```bash
TAX_DATA_WRITE_LOCALLY=1 \
TAX_DATA_USER_ID=user_test \
TAX_DATA_LOCAL_ROOT=/path/to/data/root \
Rscript src/main.R
```

The baseline runscript expects dependencies under a directory structure like:

```text
<root>/
  raw_data/
    IRS-PUF/
      v1/
        <vintage>/
          historical/
            puf_2015.csv
            demographics_2015.csv
  model_data/
    Compiled-SOI-Tables/
    Macro-Projections/
  raw_data/
    CPS-ASEC/
    DINA/
    SCF/
    SIPP/
```

The public repositories document the required modeling components, but the full
dependency bundle is not assembled inside this repo. In practice, a standalone
run needs the 2015 IRS PUF plus the additional inputs referenced by the
baseline runscript.

## Outstanding tasks
As we focus on building a usable PUF for Budget Lab launch products, we are tabling several nice-but-not-strictly-necessary features for future versions. These include:  
- Unpacking aggregate return
- Projecting censored TCJA variables for targets
- Running the LP targeting algorithm for projected years
- Less hard-coding of years. Lots of 2015- and 2017-specific code right now. Will be made flexible. 
