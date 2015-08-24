# Introduction
All the contents here applies to **Wakan 1.80.15+**

# Details
How Wakan runs is controlled by a wakan.ini file in the application directory. If this file is missing, it'll be auto-created, or you can create it by hand:
```
[General]
Install=Portable
```

Possible values for "Install" are:
  * **`Portable`** -- store all settings and user data in Wakan folder
  * **`Standalone`** -- store settings in registry and user data in roaming AppData folder
  * **`Compatible`** -- store settings in registry and all data in Wakan folder, like older versions did
  * file missing -- ask the user on the first run

When installed by an installer, Wakan is configured to run in standalone mode by default.

If you're running in compability mode, you can make the application standalone by going to Settings->Portability and running "Upgrade".

# Dictionaries
Dictionaries, imported or not, language files and other additional resources are always stored in Wakan folder at this point.

You'll need either to run as Administrator, or enable UAC, to change those from Wakan when running from Program Files folder.

# FAQ

## How do I place portable Wakan on my flash drive?
Copy Wakan to a flash drive, delete `wakan.ini` and start the application. Answer "Portable".

## How can I share Wakan between computers with Dropbox?
**Quick way**: Put Wakan folder into Dropbox, delete `wakan.ini` and choose "Portable".

**Proper way**: Install Wakan in standalone mode on all computers where you use it, then make symlinks from Dropbox to `AppData\Roaming\Wakan`.

## How can I move my existing data to Portable version?
Find where your existing data is stored (see "Where does Wakan keep my data?") and move it to `UserData\` subfolder of Wakan folder.

## Where does Wakan keep my data?
That depends on which version are you running. To see, go to `Database > Settings > Portability`.

Portable version: In Wakan folder and in `UserData\` subfolder.

Standalone version: In your `UserProfile\AppData\Roaming\Wakan`.

Older Wakan versions store some data right in Wakan folder, which on the newer systems goes to `UserProfile\AppData\Local\VirtualStore\Program Files (x86)\Wakan`. If nothing helps, check there.