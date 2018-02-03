# planetfeed

This project is for posting entries from Planet Lisp to the twitter
feed of @planet_lisp.

The original @planet_lisp setup was by Hans Hübner. His original
software is on [github](https://github.com/hanshuebner/planetwit).

## Usage

Build the "planetfeed" binary with `make`. [buildapp](https://github.com/xach/buildapp) is
required. Quicklisp is also required; if the installation is somewhere
other than the default `~/quicklisp/` directory, use `make
QUICKLISP_HOME=/path/to/quicklisp`. 

To set up planetfeed for the first time, use: `planetfeed --login
--credentials-file /path/to/file.txt`. You will be prompted to
authorize the app via an URL and PIN system. The twitter credentials
for posting will be saved in the specified credentials file. Make sure
the file is only accessible to people and processes you want to be
able to tweet.

To run planetfeed to update twitter, use `planetfeed
--credentials-file /path/to/file.txt`. planetfeed will fetch the
latest Planet Lisp feed, fetch recent @planet_lisp tweets, and tweet
any feed items that haven't been tweeted recently.

## Feedback

This code is just for me, Zach Beane, to run. But if you have
questions or comments about it, [email me](mailto:xach@xach.com) or
[make a github issue](https://github.com/xach/planetfeed/issues).

