Use Cases
==

Name: Enemy Encounter
--

Actor: Player
--

Goals: Determine outcome of enemy encounter.
--

Preconditions: Player is playing the game and is in an area with enemies and player has sensors.
--

Flows:
--
Player
**Game**

**1. Game displays text informing of enemy encounter**

2.Player decides to run or fight.

**3. Game stats fight mode.**

4.Player fights.

**5. Game displays fight outcome**

Postconditions: Player character gains items of enemy OR Player is required to restart due to player character death.
--


Name: Finding an Item
--

Actor: Player
--

Goals: Allow the player to find items in the world.
--

Preconditions: Player is playing the game and is in an area where items can be found.
--

Flows:
--
Player
**Game**

**1. Game displays text informing of the existence of a nearby item**

2.Player decides to pick up item.

**3. Game places item in players' inventory**

Postconditions: Player character gains the item.
--



Name: Equipping an Item
--

Actor: Player
--

Goals: Allow the player to equip itmes.
--

Preconditions: Player is playing the game.
--

Flows:
--
Player
**Game**

**1. Game displays text informing the player of available items**

2.Player decides to equip an item.

**3. Game equips item to player**

Postconditions: Player now has item equipped.
--



Name: Crafting a new Item
--

Actor: Player
--

Goals: Allow the player to craft new items.
--

Preconditions: Player is playing the game and has items available.
--

Flows:
--
Player
**Game**

**1. Game displays text informing the player of available items**

2.Player decides to craft a new item.

**3. Game creates new item in players inventory**

Postconditions: Player now has the new item.
--



