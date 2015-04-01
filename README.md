Welcome to the Open Bank Project Social Finance !

# ABOUT

This application demonstrates some of the principles of the Open Bank Project:

1) Sliding scale of privacy and disclosure. e.g. Use aliases to protect real names but still show the flow of money .
2) Different views on account data (Public / Share holders / Team etc.) e.g. hide balances on public view.
3) Comment on transactions
4) Add other meta data e.g. tags / images to transactions / payees.

The project roadmap is available [here.](https://trello.com/b/aBELCLYA/open-bank-project-sofi) 

# LICENSE

This project is licensed under the AGPL V3 (see NOTICE) and a commercial license from TESOBE.

# SETUP

The project is using sbt or Maven 2 as a build tool.
See build.scala or pom.xml respectively for the dependencies.

----

To compile and run jetty, cd into the root directory (where this file is) and run:

$ sbt
...
> compile
> ~;container:start; container:reload /

(Note that you first have to start sbt and then on its console start jetty with the container:start task, otherwise it will exit immediately. More here: https://github.com/siasia/xsbt-web-plugin/wiki)

In OS X, sbt can be installed with $ sudo port install sbt

----


Alternatively, maven can also be used:

mvn jetty:run


Note: You may need to add the pluginGroup to the $HOME/.m2/settings.xml

<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0
                      http://maven.apache.org/xsd/settings-1.0.0.xsd">
  ...
  <pluginGroups>
    <pluginGroup>org.mortbay.jetty</pluginGroup>
  </pluginGroups>
  ...
</settings>

---

# PROPS FILE

There is a props file template provided at src/main/resources/props/sample.props.template. It needs to be renamed and modified in order for
the application to work.

1. Renaming:

The sample.props.template file must be renamed for Lift to find it (https://www.assembla.com/wiki/show/liftweb/Properties). Renaming it to default.props
should be the easiest way to get started.

2. Filling in values:

transloadit.authkey
transloadit.addImageTemplate

The Social Finance app uses transloadit.com to process uploaded images. If these are not filling in, users will not be able to add images to
individual transactions.

transloadit.authkey can be obtained by registering at transloadit.com
transloadit.addImageTemplate can be obtained by creating a template at transloadit. A sample template which resizes images to 250px width is :

{
  "steps": {
    "resize_to_250": {
      "robot": "/image/resize",
      "width": 250,
      "zoom": false
    }
  }
}

*base_url*

The base_url is used to calculate the callback url to give to the Open Bank Project API server. This should just be the
base url used to access the social finance application. So if you're running a copy of the social finance application at
sofi.example.com over https, on the standard port, it would be "https://sofi.example.com".
An example value for local development could be: http://127.0.0.1:8080 (as 8080 is the default Lift development port)

*api_hostname*

The api_hostname should be the base url of the Open Bank Project API. For example, https://api.openbankproject.com/api

*obp_consumer_key*
*obp_secret_key*

The keys are obtained by registering as a developer on the Open Bank Project API server located at "api_hostname".


All in all, a props file could look something like:

transloadit.authkey=89hs8fho98fsho8hsf48sfo98sh
transloadit.addImageTemplate=s9fe8sh8h4sof98hf84s8fs48f4
api_hostname=https://api.openbankproject.com/api
obp_consumer_key=uodsifnodsfifdsliufdsliufdsfdsfsdfsx
obp_secret_key=iuesbfiyvglxzgifg7eisgei7fglesfi
base_url=http://localhost:8080
