#---------------------------------------------
# record_ids.R
#
# The reserved id block for Forbes synthetic records (S25), shared by
# forbes_splice.R, which makes the ids, and main_state_weights.R, which tells
# Forbes records from the rest by them. Layout and its asserted bounds:
# forbes_splice.R, make_forbes_id().
#---------------------------------------------

FORBES_ID_BASE   = 9000000L   # first id in the reserved block
FORBES_ID_STRIDE = 10000L     # ids per list year (largest list seen: 935)
FORBES_ID_EPOCH  = 2020L      # list-year origin for the block offset
