module Story
  ( Event(..)
  , eventName
  , story
  ) where

import Items

type Dictionary = [(Event, String)]

data Event
  = ItemPickup Item String
  | ItemDrop Item String
  | StoryEvent String

instance Eq Event where
  (ItemPickup _ s) == (ItemPickup _ t) = s == t
  (ItemDrop _ s) == (ItemDrop _ t) = s == t
  (StoryEvent s) == (StoryEvent t) = s == t
  _ == _ = False

-- Get the event's name
eventName::Event->String
eventName (ItemPickup _ s) = s
eventName (ItemDrop _ s) = s

story::Event->Maybe String
story event = slookup eventDictionary event

eventDictionary :: Dictionary
eventDictionary = [

  (StoryEvent "intro",
    "With a burst of electricity, you awaken. You are a robot. You know this"
    ++ " becuase you hava an identifySelf(); function. You are modular. You have"
    ++ " four slots to equip various items. Some would mistake these as two \"arm\" "
    ++ " slots and two \"leg\" slots, but you know better. You could equip anything to these slots."
  ),

  (StoryEvent "start",
   "You are alone in a dark place. Fortunately, you currently have a flash light equipped in one slot."
  ),

  (StoryEvent "use flashLight",
   "You turn on your flash light. You see a tool arm on the ground. It has various cool tools you might want to use."
  ),

  (StoryEvent "attach arm",
   "You attach that arm to a free slot. You feel great!"
  ),

  (StoryEvent "slots full",
   "All your arm slots are full. You can remove an old one if you want to."
  ),

  (StoryEvent "flash drive found",
   "There is a usb flash drive sitting on the ground next to you. Normally you wouldn't trust a random flash drive, but something about this one looks comforting, and trustworthy."
  ),

  (StoryEvent "plug flash drive in",
   "You plug the flash drive into a spare usb 3.0 slot on your head. Immediately, apop-up flashes across your vision. It says, simply, \"Would you like to install Linux?\""
  ),

  (StoryEvent "virus appears",
   "A wild virus appears! It identifies itself as \"evilVirus32.exe\". "
  ),

  (StoryEvent "linux installed",
   "You successfully installed Linux on yourself! That was far easier to do than you had heard. You are now immune to the virus that suddenly appears, trying to infect your system."
  ),

  (StoryEvent "game finished",
   "With a burst of electricity, the computer virus disappears. You are safe. For now."
  )

  ]

slookup::Dictionary->Event->Maybe String
slookup [] _ = Nothing
slookup ((event, str):dict) key = if key == event
  then Just str
  else slookup dict key
