# Arciorgano Player Patch
_Remote access to the Arciorgano_

The Arciorgano Player Patch is a collection of tools to remotely play the Arciorgano, located at the Campus of the Musik-Akademie Basel (FHNW). 

The Arciorgano Player Patch is run on PureData (Pd) on the user's computer. It communicates via an internet connection (OSC messages) or a USB cable (serial port) with the Arciorgano. The patch can also be used without any connection to the Arciorgano, in this case the patch can simulate the sound of the organ with various sample sets. 



## Set up the Player Patch

1. Download and install PureData: https://puredata.info/downloads (Please use the "vanilla" version)

2. Download the latest version of the Player Patch: https://gitlab.fhnw.ch/johannes.keller/arciorgano-pd-sender/-/releases

3. Unzip all the files into a folder of your choice. 


## Install the sample files

The Player Patch doesn't include the sample libraries of the Arciorgano. These need to be downloaded and installed manually. There are samples available with different tuning systems. Choose the system you intend to use and download the .zip file.

- "mode1" (Vicentino's enharmonic system, based on a meantone chain of fifths): https://drive.google.com/file/d/1Rk9sq8aGRtnBpBI20V9piY4cyc1Wcq-F/view?usp=sharing

- "mode2" (Vicentino's 'adaptive-just' system. The upper manual is providing pure fifths and minor thirds to the meantone lower manual):https://drive.google.com/file/d/1RwbwqGZj4EcBwztgxwGcjCKDKOUcyJKc/view?usp=sharing

- "mode3" (Johannes Walter's 'pseudorein', based on pure fifths, where the Pythagorean diminished fourths is used as a pseudopure major third): https://drive.google.com/file/d/19md4GUvS_xAUaLa_51cxX_VGOw_bjDyF/view?usp=sharing

- "mode7" (a limit5-Tonnetz inspired by Salina's 24-division system): https://drive.google.com/file/d/1qx5avr9LAlDW6shJgt6_xfhyKvIF8W19/view?usp=sharing

- "mode8" (a complex limit-7 Tonnetz): https://drive.google.com/file/d/1afSW9_2PTBl1o_7PqfBw__OOvRECXmWS/view?usp=sharing


Create a subfolder in the Arciorgano Player Patch named 'samples'. Unzip the downloaded sample collections into this subfolder. Don't change the folder structure within the .zip-file. 

To check: the sample for the lowest note in 'mode1' should have the following file path:

.../arciorgano-pd-sender-v[x]/samples/mode1/mode1-export-wav-neumann/mode1-1.wav


## Connect to the Arciorgano

### Via OSC (internet)

For the time being, you need a VPN connection to the servers of the Fachhochschule Nordwestschweiz (vpn.fhnw.ch). Additionally, you need a user ID for the Arciorgano that can be requested from Johannes Keller: johannes.keller@fhnw.ch

1. Establish a VPN connection (for example with the Cisco Anyconnect client).

2. Open the Arciorgano Player Patch in PureData. 

3. At the bottom of the main window you find the "OSC connection" box. Type in your user ID and click on "connect to arciorgano". You should be able to play the organ by clicking on the keyboard schemata or the pipe schemata. 


### Via USB (serial)

In case you have physical access to the Arciorgano, you can use the USB connection to the player module. This is recommended for performance situations.

1. Plug the USB cable into your computer.

2. Open the Arciorgano Player Patch in PureData. 

3. At the bottom of the main window you find the "serial output" box. Toggle on "active" and type in the "device name" of your serial port. This depends on your operating system, for MacOS and Linux you can use the terminal command "ls /dev/tty*" to see the available ports. Click on "reconnect". The organ should now be playable from the Patch. 



## Use the eartraining module

Follow the steps above to install the PureData software, the Arciorgano Player Patch and the sample set 'mode1'.

![Setting up the Eartrainer](/doc/screenshot-eartraining-preparation.png)

1. Open the Arciorgano Player Patch in PureData.

2. Switch on "DSP" in the PureData main window. (You might want to check the audio device settings, see menu -> Media -> Audio Settings).

3. Toggle on "active" in the "sampler" box.

4. Click on "mode 1 / neumann" in the "sampler" box. The Patch should now simulate the organ sound. Test it by clicking on the keyboard or pipe schematas. Don't use any other "modes" for the Eartrainer.

5. Click on the "pd eartrainer" element underneath the "sampler" box. The Eartrainer window should now be visible.

6. Train your ears:

   - Click on "new quiz" to listen to an interval. Repeat the same interval by clicking on one of the three "playback" buttons.

   - On the keyboard schemata in the Eartrainer window you see the first note of the interval marked with an X. Click on the key representing the second note of the interval. If you would like to see the solution, click on "print solution". Both notes will be marked on the keyboard, and the names of the interval and both notes will be printed in the PureData window.

   - To train only specific intervals, use the "interval pool" section. To select the range that is used, change the slider positions. 

   - In the main Patch window, you will always see the notes that are currently played. Hide this window to avoid seeing the solution by accident. You can play the organ by clicking on the keyboard in the main Patch window without interfering with the quiz process. You can use this possibility to compare different intervals while looking for the right answer to the quiz.
   
![Using the Eartrainer](/doc/eartraining-window.png)

## Use MIDI keyboards to play the Patch

You can connect one or two standard MIDI keyboards to the Arciorgano Player Patch. 

1. Plug in your MIDI controllers.

2. Toggle on "acitve" in the "MIDI controller A" and "MIDI controller B" boxes in the main patch window. 

3. Check the MIDI input settings in menu -> Media -> MIDI Settings. Select your keyboards as input devices. You might want to activate "Use Multiple Devices".

4. The patch should now receive and process MIDI signals. Adapt the settings in the "MIDI controlles" box in the main patch window. The black keys are split by velocity, low velocity (soft touch) will trigger the front part of the upper keys, high velocity (strong touch) will trigger the back part. Adapt the split velocity value to meet your touch and hardware configuration. 



## Use notation or audio software to play the Patch

It is possible to play the Patch from software like Sibelius or Reaper. You need to set up a MIDI routing within your system. On MacOS this can be achieved by activating the IAC MIDI driver (to be found in "Audio settings" under "MIDI studio" in the menu. Activate the IAC driver. On Windows you need a software tool, such as LoopMIDI. On Linux, using a Jack audio server is recommended. Set up your software to use this virtual MIDI device for MIDI output.

1. Open the Arciorgano Player Patch in PureData.

2. Check the MIDI settings: menu -> Media -> MIDI settings. Select your virtual MIDI device as "Input Device".

3. Toggle on "active" in one of the "bridge" boxes in the main patch window:
   - "Sibelius bridge" uses PitchBend commands to encode enharmonic alterations. To add these commands to a score you need to install the Sibelius Plugin "studio31-ReadTabulature.plg" that is located in the Patch folder (see "sibelius-plugin" subfolder). Check the Sibelius online documentation how to install a plugin locally on your system http://www.sibelius.com/download/plugins/index.html?help=install
   - "Reaper bridge" uses MIDI channels 1 and 2 to encode the two manuals separately. 
   - "Vicentino bridge" is under construction, don't use it.
   - "Velocity bridge" uses Velocity values to encode enharmonic alterations. It is the recommended way to transfer MIDI data between Sibelius, Reaper and the Patch. To encode a Sibelius score for this "bridge", please install the Sibelius plugin "arciorganoVelocityPlayback.plg". 

4. In case you use Sibelius: select the parts of the score that you want to send to the Patch. Run one of the mentioned plugins to add the necessary MIDI commands. Play back the score.

5. The Patch should now react to MIDI output from Sibelius or Reaper (or any other MIDI software that has been configured correctly). 




## Listen to the organ

For remote work sessions, the organ can be monitored via an audio livestream. The live audio stream is generated by an Icecast 2 server, at 10.209.132.109:8080. To access it, you could type this address into your browser address bar: http://10.209.132.109:8080. You need an active VPN connection (vpn.fhnw.ch) to access the audio server. 

If the server is active, you should see the details of "Mount Point /pandora.mp3". Click on the M3U symbol, top right. The browser should start playing the stream, or offer you options to open the stream with an application. If you don't have a player software capable of doing that, you might want to install VLC (https://www.videolan.org/vlc/).



