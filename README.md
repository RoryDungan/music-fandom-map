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

Secondly, you will need a [Last.fm API key](https://www.last.fm/api/account/create) in order to pull description and image data from Last.fm. Once you have obtained this, add it in `data-wrangler.conf` under `lastfm_api_key`.

Run `stack exec data-wrangler` to populate the database with data from Spotify. Spotify's charts are updated daily so if you want your data to be kept continuously up to date you can set this up to run in a CRON job.

Run `stack exec server` to start the server. 

Once it has booted up you should see the following message in the console:
```
$ stack exec server
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

## Deploying

The full Haskell Stack install is quite large (around 1.5GB) and requires around 2.5GB of RAM to build the project. Unless you want to host the app on an especially beefy server, I'd recommend building it locally first and then copying the binaries to your server.

First build the frontend in production mode:

    pushd frontend
    npm install
    ng build --target=production
    popd

Build the server:

    stack build

We can locate the binaries with `stack exec which data-wrangler` and `stack exec which server`. You could just copy the binaries manually, but an easy way to move everything from a build machine to the production machine is to compress all the files needed to run the app into a tarball:

    tar -cvJf music-fandom-map.tar.xz frontend/dist/ country-codes.csv data-wrangler.conf -C `dirname \`stack exec which data-wrangler\`` data-wrangler server

This will create a single file that we can copy to the server and then set up:

    # Extract the archive
    tar -xvf music-fandom-map.tar.xz
    
    # Run the data wrangler
    ./data-wrangler

    # Start the server!
    ./server

Don't forget that this also requires MongoDB to be available on the server, and you must specify your Last.fm API key in `data-wrangler.conf`. 
