# Geocoding on Google maps if you have an API key or else use Dr. Chuck's
# static subset of the data.

import urllib.request, urllib.error, urllib.parse
import json
import ssl

API_KEY = False

if API_KEY is False:
    API_KEY = 42
    SERVICE_URL = "http://py4e-data.dr-chuck.net/json?"
else:
    SERVICE_URL = "https://maps.googleapis.com/maps/api/geocode/json?"

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

print("\nEnter 'e' to exit the program.")
while True:
    address = input("Enter location: ")
    if address == "e":
        exit()

    parms = {}
    parms["address"] = address
    if API_KEY is not False:
        parms["key"] = API_KEY
    url = SERVICE_URL + urllib.parse.urlencode(parms)

    print("Retrieving", url)
    urlHand = urllib.request.urlopen(url, context=ctx)
    urlData = urlHand.read().decode()
    print("Retrieved", len(urlData), "characters")

    try:
        urlDataDict = json.loads(urlData)
    except:
        urlDataDict = None

    if (
        urlDataDict is None
        or "status" not in urlDataDict
        or urlDataDict["status"] != "OK"
    ):
        print("==== Failed to retrieve ====")
        print("Status: ", urlDataDict["status"], "\n")
        continue

    # print all data
    # print(json.dumps(urlDataDict, indent=4))

    lat = urlDataDict["results"][0]["geometry"]["location"]["lat"]
    lng = urlDataDict["results"][0]["geometry"]["location"]["lng"]
    print("Latitude, Longitude:", lat, lng)
    print("Address:", urlDataDict["results"][0]["formatted_address"])

    # Print the country short name and not when the location is in no country.
    addComp = urlDataDict["results"][0]["address_components"]
    for comp in addComp:
        if "country" not in comp["types"]:
            continue
        print("Country:", comp["short_name"])
    print()
