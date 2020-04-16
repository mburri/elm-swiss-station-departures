FROM node:11.9.0-alpine as builder

RUN apk update && apk add make curl bash && rm -rf /var/cache/apk/*

WORKDIR /usr/src/app

COPY . .
RUN make prod

FROM bitnami/nginx:1.14.2-debian-9-r59

COPY --from=builder /usr/src/app/dist /app
