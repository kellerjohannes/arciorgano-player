# arcicaster
_Remote accces to the Arciorgano_

Arcicaster is a collection of tools to remotely play and monitor the Arciorgano, located
at the Campus of the Musik-Akademie Basel (FHNW). 

This document describes a rudimentary setup which can be extended easily. The player
interface of the organ is listening to OSC messages that can be sent from any software
that supports OSC. Simultaneously there is a live audio stream available to listen to the
organ. 

## Quick start

### Listen to the organ

The live audio stream is generated by an Icecast 2 server, at 10.209.132.109:8080. To
access it, you could type this address into your browser address bar:
http://10.209.132.109:8080. If the server is active, you should see the details of "Mount
Point /pandora.mp3". Click on the M3U symbol, top right. The browser should start playing
the stream, or offer you options to open the stream with an application. If you don't have
a player software capable of doing that, you might want to install VLC
(https://www.videolan.org/vlc/).

### Play the organ

1. Check if PureData is installed on your system. If not: download and install PureData on
   your system. The "Vanilla" release should be sufficient.
   https://puredata.info/downloads

2. Open the file arciorgano-player.pd from this repository in PureData. You should see a
   clickable keyboard and pipe arrangement of the Arciorgano.

3. Click on the green button "connect to arciorgano".

4. Clock on the green button "motor on". The blower should now start, wait a couple of
   seconds until the wind pressure is built up.

5. Play the organ by clicking on the keys or pipes. 


