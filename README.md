# ABOUT

The Sofit application demonstrates some of the social aims behind the Open Bank Project:

1) Sliding scale of privacy and disclosure. e.g. Use aliases to protect real names but still show the flow of money .
2) Different views on account data (Public / Share holders / Team etc.) e.g. hide balances on public view.
3) Comment on transactions
4) Add other meta data e.g. tags / images to transactions / payees.


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

## Ubuntu

If you use Ubuntu (or a derivate) and encrypted home directories (e.g. you have ~/.Private), you might run into the following error when the project is built:

    uncaught exception during compilation: java.io.IOException
    [ERROR] File name too long
    [ERROR] two errors found
    [DEBUG] Compilation failed (CompilerInterface)

The current workaround is to move the project directory onto a different partition, e.g. under /opt/ .


# PROPS FILE

There is a props file template provided at src/main/resources/props/sample.props.template. It needs to be renamed and modified in order for
the application to work.

1. Renaming:

The sample.props.template file must be renamed for Lift to find it (https://www.assembla.com/wiki/show/liftweb/Properties). Renaming it to default.props
should be the easiest way to get started.

2. Filling in values: (This step can skip)

transloadit.authkey
transloadit.addImageTemplate

The Sofit app uses transloadit.com to process uploaded images. If these are not filling in, users will not be able to add images to
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

## These change will be performe in default.props file (/src/main/resources/props/default.props).  

### *base_url*

The base_url is used to calculate the callback url to give to the Open Bank Project API server. This should just be the
base url used to access the social finance application. So if you're running a copy of the Sofit application at
sofit.example.com over https, on the standard port, it would be "https://sofit.example.com".
An example value for local development could be: http://localhost:8081 (as 8081 is the default Lift development port)

### *api_hostname*

The api_hostname should be the base url (https://api.openbankproject.com) of the Open Bank Project API. If, Sofit is running locally then define api_hostname as http://127.0.0.1:8080 in default.props file. For this purpose, OBP-API must be running locally, without it is Sofit will not work. 

### *obp_consumer_key* \
### *obp_secret_key*

The keys are obtained by registering as a developer on the Open Bank Project API server (https://apisandbox.openbankproject.com/) located at "api_hostname". For this, Register as a user, after registration then click on *Get API Key*. Next, important step is, in Redirect URL (Optional) give Sofit’s base url (http://localhost:8081/) , if sofit is running locally. 


All in all, a props file could look something like:

transloadit.authkey=89hs8fho98fsho8hsf48sfo98sh\
transloadit.addImageTemplate=s9fe8sh8h4sof98hf84s8fs48f4\
api_hostname=https://api.openbankproject.com/api \
defaultAuthProvider=https://api.openbankproject.com/api \
obp_consumer_key=uodsifnodsfifdsliufdsliufdsfdsfsdfsx\
obp_secret_key=iuesbfiyvglxzgifg7eisgei7fglesfi\
base_url=http://localhost:8080 
