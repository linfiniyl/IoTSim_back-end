package com.IoTSim.management_server.api.security.service;


import com.IoTSim.management_server.context.user.model.User;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@Service
@Slf4j
public class JwtService {
    @Value("${token.signing.key}")
    private String SECRET_KEY;

    public String extractUsername(String token){
        log.debug("Extracting username from token");
        return extractClaim(token, Claims::getSubject);
    }

    public String generateAccessToken(UserDetails userDetails){
        log.debug("Generating access token for user: {}", userDetails.getUsername());
        Map<String, Object> claims = new HashMap<>();
        if (userDetails instanceof User customUserDetails) {
            claims.put("id", customUserDetails.getId());
            claims.put("role", customUserDetails.getRole().name());
        }
        String token = Jwts.builder()
                .claims(claims)
                .subject(userDetails.getUsername())
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .expiration(new Date(System.currentTimeMillis() + 1000 * 60 * 24))
                .signWith(getSignInKey(), SignatureAlgorithm.HS256)
                .compact();
        log.info("Access token generated for user: {}", userDetails.getUsername());
        return token;
    }

    public String generateRefreshToken(UserDetails userDetails){
        log.debug("Generating refresh token for user: {}", userDetails.getUsername());
        Map<String, Object> claims = new HashMap<>();
        if (userDetails instanceof User customUserDetails) {
            claims.put("id", customUserDetails.getId());
            claims.put("role", customUserDetails.getRole().name());
        }
        final LocalDateTime now = LocalDateTime.now();
        final Instant refreshExpirationInstant = now.plusDays(30).atZone(ZoneId.systemDefault()).toInstant();

        String token = Jwts.builder()
                .claims(claims)
                .subject(userDetails.getUsername())
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .expiration(Date.from(refreshExpirationInstant))
                .signWith(getSignInKey(), SignatureAlgorithm.HS256)
                .compact();
        log.info("Refresh token generated for user: {}", userDetails.getUsername());
        return token;
    }

    public boolean isTokenValid(String token, UserDetails userDetails){
        log.debug("Validating token for user: {}", userDetails.getUsername());
        String username = extractUsername(token);
        boolean valid = (username.equals(userDetails.getUsername()) && !isTokenExpired(token));
        log.info("Token validation result for user {}: {}", userDetails.getUsername(), valid);
        return valid;
    }

    private boolean isTokenExpired(String token) {
        boolean expired = extractExpiration(token).before(new Date());
        log.debug("Token expired status: {}", expired);
        return expired;
    }
    public boolean verifyToken(@NonNull String token){
        log.debug("Verifying token");
        try {
            Jwts.parser()
                    .setSigningKey(getSignInKey())
                    .build()
                    .parseClaimsJws(token);
            log.info("Token verified successfully");
            return true;
        } catch (Exception e) {
            log.error("Token verification failed: {}", e.getMessage());
            return false;
        }
    }
    private Date extractExpiration(String token) {
        log.debug("Extracting expiration from token");
        return extractClaim(token, Claims::getExpiration);
    }

    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver){
        log.debug("Extracting claims from token");
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    public Claims extractAllClaims(String token){
        log.debug("Extracting all claims from token");
        return Jwts.parser()
                .setSigningKey(getSignInKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    private SecretKey getSignInKey() {
        byte[] keyBytes = Decoders.BASE64.decode(SECRET_KEY);
        return Keys.hmacShaKeyFor(keyBytes);
    }
}
