# thingspeakR
A proto-package for reading and archiving data from www.thingspeak.com

Our hardware team streams remote sensor data via the IoT service ThingSpeak. These scripts retrieve that data for long-term storage and analysis.

These demonstrate:
 - pulling a user's list of channels from the API
 - safely parsing the JSON responses
   - storing failed parses externally for inspection
   - alerting the software lead via IFTTT that there was a problem
 - pulling feeds from each channel
 - reconciling existing data with any potential new entries
 - automating all of it hourly on a Windows machine
