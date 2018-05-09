# Info for developers

Some useful hints to make development of the project easier.

## Adding new `AppState` field

Field such as `baseKey`, `basePitch`, `octave`, etc.

Last update of this instruction was in **29.04.2018**.

When you add new app state field you'd probably need to handle it in different modules, such
`MIDIPlayer` or `GUI`. Here's a list of places you probably would handle your new field:

1. `EventHandler` module:
   1. Extend `AppState` data type with your new field;
   2. You may need to add a constructor of `EventToHandle` to change a value of your new field;
   3. Add default value of your new field to `Default AppState` instance;
   4. Add transform for new value of your new field to `runEventHandler`;
   5. If your field could shift current pitch:
      1. Check if it's changed in `updateStateMiddleware` to update pitch mapping;
      2. Add shift logic to `getPitchMapping`.
2. `Main` module:
   1. Add handler to `evListener` (usually it updates a value in GUI),
      don't forget to update key mapping by sending `SetPitchMapping` event to the `GUI`
      (if your new field could affect key mapping);
   2. If GUI supposed to have representation of your new field:
      1. Add initial value to `guiInitValues`;
      2. Add GUI user interaction handler to `guiIface` that will trigger change to `EventHandler`.
3. `GUI` module (if your new field has GUI representation):
   1. Add field of handler to `GUIContext` data type which would handle of updating your value from
      `GUI` to somewhere else (when value changed from `GUI` this handler will be called)
      (you may not need this if a value can't be changed from the `GUI`);
   2. Call that handler from some `GUI` element change handler
      (you may not need this if a value can't be changed from the `GUI`);
   3. Add `GUI` representation of your field to `GUIState` data type;
   4. Use that `GUI` state field somewhere to show it to the user;
   5. Extend `GUIStateUpdate` data type with setter of new value for your field to update a value in
      `GUI` when it changes from somewhere else;
   6. Go to place starts with `takeMVar stateUpdateBus` where these `GUI` state updates are handled
      and add proper handler for update of your new field value.
