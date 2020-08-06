FROM node:12.16.2 as node
LABEL Description="VMware Blockchain UI"

WORKDIR /app

COPY ./ /app/

RUN npm install
RUN node patch.js

ARG env=prod
ARG UI_BUILD=build:prod:csp

RUN npm run $UI_BUILD


# Stage 1, based on Nginx, to have only the compiled app, ready for production with Nginx
FROM nginx:1.17

COPY --from=node /app/dist/ /usr/share/nginx/html

COPY ./nginx-custom.conf /etc/nginx/conf.d/default.conf
