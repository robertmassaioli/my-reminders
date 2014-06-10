#!/bin/bash -e

PGPASSWORD=pingme psql --host localhost -U pingme
