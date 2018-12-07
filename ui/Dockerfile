FROM node:8.9.4 as node
LABEL Description="VMware Blockchain UI"

WORKDIR /app

COPY .npmrc /app/
COPY package-lock.json /app/
COPY package.json /app/

RUN npm install

COPY ./ /app/

ARG env=prod

RUN npm run build:prod


# Stage 1, based on Nginx, to have only the compiled app, ready for production with Nginx
FROM nginx:1.13

COPY --from=node /app/dist/ /usr/share/nginx/html

COPY ./nginx-custom.conf /etc/nginx/conf.d/default.conf
