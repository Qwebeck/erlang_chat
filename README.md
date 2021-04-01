# Summary

A training project to develop functional programming skills. 
The main idea is to create a server for the chat application that allows users to create their 
rooms and join existing to chat with other people.

# Architecture
The application consists of services, which communicate with each other through a message-passing mechanism provided by Erlang/OTP.
There exist two types of services, each of which handles one responsibility critical for correct application work:

1. User services
2. Messaging room service

Both services share a common structure, that consists of two main processes:

1. Service supervisor - a process responsible for passing messages between the main application and processes responsible for service logic and service error handling. 
2. Service logic process - a process responsible for one activity related to service type (e.g for user service, service logic process could perform such tasks as creating user, changing username, setting the password, etc.)

Below diagram shows an example of communication between subprocesses in User service

<img src="https://i.imgur.com/ocAiOMx.png" />
