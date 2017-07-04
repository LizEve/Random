#!/bin/bash

message="Adding files"

echo $message

git add *
git commit -a -m "$message"
git push
