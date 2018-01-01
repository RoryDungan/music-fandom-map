# music-fandom-map

Interactive maps showing which countries artists are most popular in. Inspired by [What Music Do Americans Love the Most?](https://www.nytimes.com/interactive/2017/08/07/upshot/music-fandom-maps.html) from the New York Times.

## Getting started

There are two main parts to the project: server and data-wrangler. The data-wranger downloads data for the current top 200 tracks for each country from [Spotify](https://spotifycharts.com/regional), processes it and stores it in a MongoDB database. The server hosts the web frontend and provides a simple REST api that it can use to request data from the database.

## Building

First, make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then run `stack build` to build the project.
This will also download the correct version of the Haskell compiler and all dependencies. 

## Building frontend

The web frontend is stored in a Git submodule in the `frontend` directory. In order to build it you will need [NPM](https://www.npmjs.com/) and the [Angular CLI](https://github.com/angular/angular-cli), which can be installed via `npm install -g @angular/cli`.

Run `npm install` to restore all the dependencies of the frontend.

Run `ng build` to build.

## Setting up and running
First, make sure you have a [MongoDB](https://www.mongodb.com) instance installed and running on your system.

Run `stack exec data-wrangler` to populate the database with data from Spotify. Spotify's charts are updated daily so if you want your data to be kept continuously up to date you can set this up to run in a CRON job.

Run `stack exec server` to start the server. 

Once it has booted up you should see the following message in the console:
```
$ stack exec server
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```