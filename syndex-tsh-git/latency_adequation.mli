module Make :
  functor (Progress_box : Progress_box.PROGRESS_BOX_SIG) ->
    sig
      val schedule_length : float ref
      val schedule_reliability : float ref
      val schedule_big_lambda : float ref
      val adequation : string -> Adequationtypes.graph_type ->
        Adequationtypes.graph_type * Adequationtypes.full_schedule_type
    end
