type Key = String
type Event = String
type Dictionary = [(Key, Event)]

eventDictionary :: Dictionary
eventDictionary = [

  ("Intro",
    "With a burst of electricity, you awaken. You are a robot. You know this"
    ++ " becuase you hava an identifySelf(); function. You are modular. You have"
    ++ " four slots to equip various items. Some would mistake these as two \"arm\" "
    ++ " slots and two \"leg\" slots, but you know better. You could equip anything to these slots."
  ),

  ("Start",
   "You are alone in a dark place. Fortunately, you currently have a flash light equipped in one slot."
  ),

  ("Use FlashLight",
   "You turn on your flash light. You see a tool arm on the ground. It has various cool tools you might want to use."
  ),

  ("Attach Arm",
   "You attach that arm to a free slot. You feel great!"
  ),

  ("Slots Full",
   "All your arm slots are full. You can remove an old one if you want to."
  ),

  ("Flash Drive Found",
   "There is a usb flash drive sitting on the ground next to you. Normally you wouldn't trust a random flash drive, but something about this one looks comforting, and trustworthy."
  ),

  ("Plug Flash Drive In",
   "You plug the flash drive into a spare usb 3.0 slot on your head. Immediately, apop-up flashes across your vision. It says, simply, \"Would you like to install Linux?\""
  ),

  ("Virus Appears",
   "A wild virus appears! It identifies itself as \"evilVirus32.exe\". "
  ),

  ("Linux Installed",
   "You successfully installed Linux on yourself! That was far easier to do than you had heard. You are now immune to the virus that suddenly appears, trying to infect your system."
  ),

  ("Game Finished",
   "With a burst of electricity, the computer virus disappears. You are safe. For now."
  )

  ]



getEventFromKey :: Dictionary -> Key -> Event
getEventFromKey ((k, e):d) key = if key == k
  then e
  else getEventFromKey d key
