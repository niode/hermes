
type Key = String
type Event = String
type Dictionary = [(Key, Event)]

eventDatabase :: Database
eventDatabase = [
("Intro",
"With a burst of electricity, you awaken.You are a robot.You know this becuase you hava an identifySelf(); function.You are modular. You have four slots to equip various items. Some would mistake these as two \"arm\" slots and two \"leg\" slots, but you know better. You could equip anything to these slots."
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
)
]
