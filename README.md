# music-fandom-map

Interactive maps showing which countries artists are most popular in. Inspired by [What Music Do Americans Love the Most?](https://www.nytimes.com/interactive/2017/08/07/upshot/music-fandom-maps.html) from the New York Times.

# Getting started

There are two main parts to the project: server and data-wrangler. The data-wranger downloads data for the current top 200 tracks for each country from [Spotify](https://spotifycharts.com/regional), processes it and stores it in a MongoDB database. The server hosts the web frontend and provides a simple REST api that it can use to request data from the database.

## Building

First, make sure you have [Stack] installed. Then build the project:
```
stack build
```
This will also download the correct version of the Haskell compiler and all dependencies. 

## Setting up and running
First, make sure you have a [MongoDB](https://www.mongodb.com) instance set up and running on your system.

To populate the database with data you will need to run the data-wrangler. Spotify's charts are updated daily so if you want your data to be kept continuously up to date you can set this up to run in a CRON job.
```
stack exec data-wrangler
```

To run the server, run the following command:
``` 
stack exec server
```

Once it has booted up you should see the following message in the console:
```
$ stack exec server
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```