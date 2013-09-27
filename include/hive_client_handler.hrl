-author('kajetan.rzepecki@zadane.pl').

%% Common client handler stuff.

%% Common client handler state record:
-record(state, {
          getter         = undefined :: function(), %% State getter function.
          setter         = undefined :: function(), %% State setter function.
          cleanup        = undefined :: function(), %% State Cleanup function.
          args           = undefined :: term(),     %% State Manager args.
          sid            = undefined :: binary(),   %% Session ID.
          hooks          = undefined :: term(),     %% External event hooks.
          event_handlers = undefined :: term(),     %% Internal event dispatchers.
          real_state     = []        :: list()      %% Client state.
         }).

