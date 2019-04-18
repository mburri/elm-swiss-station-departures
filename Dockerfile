FROM node:11.9.0-alpine as builder
WORKDIR /usr/src/app

COPY . .
RUN npm run build

FROM bitnami/nginx:1.14.2-debian-9-r59

COPY --from=builder /usr/src/app/dist /app
