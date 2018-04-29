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
   3. Add default value of your new field to `defaultAppState`;
   4. Add transform for new value of your new field to `runEventHandler`;
   5. If your field could shift current pitch:
      1. Check if it's changed in `updateStateMiddleware` to update pitch mapping;
      2. Add shift logic to `getPitchMapping`.
2. `Main` module:
   1. Add handler to `evListener` (usually it updates a value in GUI);
   2. If GUI supposed to have representation of your new field:
      1. Add initial value to `guiInitValues`;
      2. Add GUI user interaction handler to `guiIface` that will trigger change to `EventHandler`;
3. `GUI` module: **TODO**
