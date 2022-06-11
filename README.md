Welcome to Sofit
=====================

# ABOUT

The Sofit application demonstrates some of the social aims behind the Open Bank Project:

1) Sliding scale of privacy and disclosure. e.g. Use aliases to protect real names but still show the flow of money.
2) Different views on account data (Public / Share holders / Team etc.) e.g. hide balances on public view.
3) Comment on transactions
4) Add other metadata e.g. tags / images to transactions / payees.


# LICENSE

This project is licensed under the AGPL V3 (see NOTICE). Commercial licences are available from TESOBE GmbH.

# SETUP

The project is using sbt or Maven 2 as a build tool.
See build.scala or pom.xml respectively for the dependencies.

----

To compile and run jetty, cd into the root directory (where this file is) and run:

$ sbt
...
> compile
> ~;container:start; container:reload /

(Note that you first have to start sbt and then on its console start jetty with the container:start task, otherwise, it will exit immediately. More here: https://github.com/siasia/xsbt-web-plugin/wiki)

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

If you use Ubuntu (or a derivative) and encrypted home directories (e.g. you have ~/.Private), you might run into the following error when the project is built:

    uncaught exception during compilation: java.io.IOException
    [ERROR] File name too long
    [ERROR] two errors found
    [DEBUG] Compilation failed (CompilerInterface)

The current workaround is to move the project directory onto a different partition, e.g. under /opt/ .

## To run Sofit with IntelliJ IDEA

1. Make sure you have the IntelliJ Scala plugin installed.
2. Create a new folder e.g. OpenBankProject and cd there
3. git clone https://github.com/OpenBankProject/Sofit.git
4. In IntelliJ IDEA do File -> New -> Project from existing sources, navigate to the folder, and select pom.xml
5. If you see a message about an unmanaged pom.xml, click the option to let Maven manage it.
6. There is a props file template provided at src/main/resource/prop/sample.props.template. It needs to be renamed and modified or just copy and paste with a new file name for the application to work. 
7.These change will be performe in default.props file (/src/main/resources/props/default.props).  

## PROPS FILE

1. Copying and renaming:

We suggest you copy and paste the sample.props.template file so you can refer to it later.
The active Props file should be named *default.props* or another name that Liftweb accepts. See https://www.assembla.com/wiki/show/liftweb/Properties for the various options.

Below are the important Props (see example.props.tempate for more info)

### *base_url*

The base_url is used to calculate the callback url to give to the Open Bank Project API server. This should just be the
base url used to access the social finance application. So if you're running a copy of the Sofit application at
sofit.example.com over https, on the standard port, it would be "https://sofit.example.com".
The suggested value for local development is: http://localhost:8081 but obviously you can choose a different non conflicting host:port

### *api_hostname*

The api_hostname should be the base url (e.g. https://api.openbankproject.com) of the Open Bank Project API. \
If Sofit is running locally then define api_hostname as http://127.0.0.1:8080 in your props file. 

### *obp_consumer_key* \
### *obp_secret_key*

You will need to get a consumer key and secret but registering a Consumer / Get API Key on the OBP API.
See the README of OBP-API if you want to run OBP-API locally.


#OBP API Setup

Sofit needs some data setup for each User  (Each user has their own bank and the user can create historical transactions at that bank)
This is defined in OBP API function def sofitInitAction which runs after each user login.
Sofit is also relying on using some Consumer Scopes (Entitlements that are set at the Consumer level not User level)

Thus you will need to set the following on your *OBP API* (not Sofit!) Props
sofit.logon_init_action.enabled=true
allow_entitlements_or_scopes=true

Also you will need to give your Sofit Consumer the following Scopes (using the OBP API Create Scope endpoint)

CanCreateCustomerAtAnyBank
CanCreateUserCustomerLinkAtAnyBank

TODO: Consider extending sofit.login_init_action and grant CanCreateCustomer (at one bank) and CanCreateUserCustomerLink (at one bank) instead of relying on Scopes

