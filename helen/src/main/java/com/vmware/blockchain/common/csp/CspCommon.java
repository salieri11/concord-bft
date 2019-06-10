/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vmware.blockchain.common.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Csp common objects.
 */
public class CspCommon {
    /**
     * Represents Csp's Organization.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CspOrg {
        private String name;
        private String displayName;
        private String refLink;
    }

    /**
     * Hold a CSP reflink that is returned for most csp POST/PUT calls.
     */
    @Data
    public static class CspRefLinkResult {
        private String refLink;
    }

    /**
     * Represents response from csp's user's org api.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CspOrgsResult {
        private List<CspOrg> items;
    }

    /**
     * Represents response from csp's create org api.
     */
    @Data
    public static class CspOrgResult {
        private String refLink;
    }

    /**
     * Represents response from csp's import service api.
     */
    @Data
    public static class CspServiceDefinitionResult {
        private String refLink;
    }

    /**
     * Represents response from creating subscription.
     */
    @Data
    public static class CspCreateSubscriptionResponse {
        private String refLink;
    }

    /**
     * Represents response from csp's service register api.
     */
    @Data
    public static class CspServiceLinkResult {
        private String serviceLink;
    }

    /**
     * Represents response for subscription task.
     */
    @Data
    public static class CspSubscriptionTaskResponse {
        private String taskStage;
        private String subStage;
        private String failureMessage;
        private String businessPlanLink;
        private String serviceSubscriptionLink;
        private String serviceDefinitionLink;
        private String orgLink;
    }

    /**
     * Represents service instance in csp.
     */
    @Data
    public static class ServiceInstance {
        private String instanceName;
        private String serviceDefinitionLink;
        private String publicUrl;
    }

    /**
     * Represents response from csp's user validation api.
     */
    @Data
    public static class CspUser {
        private String firstName;
        private String username;
        private String lastName;
        private String password;
        private String refLink;
    }

    /**
     * Represents body for logging in via username/password OR API Key.
     */
    @Data
    public static class CspLogin {
        private String username;
        private String password;
        private String key;
    }

    /**
     * Represents response from csp's service client access key validation.
     */
    @Data
    public static class CspAccessKey {
        private String key;
    }

    /**
     * Represents response from csp's published service api.
     */
    @Data
    public static class CspPublishedService {
        private String name;
        private String accountOwnerLink;
        private String healthCheckPattern;
    }

    /**
     * Represents response from csp's provisioned service api.
     */
    @Data
    public static class CspProvisionedService {
        private String serviceName;
        private String serviceDefinitionLink;
        private Boolean internal;
    }

    /**
     * Represents response from csp's billing plans for service api.
     */
    @Data
    public static class CspBillingPlansResult {
        private List<String> refLinks;
    }

    /**
     * Represents the post body for subscription fulfillment from csp.
     */
    @Data
    @JsonInclude(Include.NON_NULL)
    @EqualsAndHashCode(callSuper = true)
    public static class CspSubscriptionFulfillmentRequest extends CspSubscriptionCallback {

        private Map<String, String> context;
    }

    /**
     * Represents the post body for subscription status notification from csp.
     */
    @Data
    @JsonInclude(Include.NON_NULL)
    public static class CspSubscriptionStatusChangeNotificationRequest {
        String subscriptionId;
        String orgId;
        String previousSubscriptionStatus;
        String currentSubscriptionStatus;
        private Map<String, String> context;
    }

    /**
     * Represents the post body for starting subscription in csp.
     */
    @Data
    public static class CspStartSubscriptionRequest {
        private String serviceDefinitionLink;
        private SubscriptionPeriod period;
        private String businessPlanLink;
        private String orgLink;
        private String paymentMethod = "CREDIT_CARD";
        private Map<String, String> context;
    }

    /**
     * Represents subscription period for billing plan.
     */
    @Data
    public static class SubscriptionPeriod {
        private int interval;
        private ChronoUnit intervalUnit;
    }

    /**
     * Represents the response for csp list service definition API.
     */
    @Data
    @NoArgsConstructor
    public static class ServiceDefinitions {
        private List<String> serviceDefinitionLinks;
        private List<ServiceDefinitionExpandedResult> results;
    }

    /**
     * Holds the expanded service definition result returned by CSP.
     */
    @Data
    @NoArgsConstructor
    public static class ServiceDefinitionExpandedResult {

        public String name;
        public String displayName;
        @JsonProperty("healthCheckURL")
        public String healthCheckUrl;
        public String documentSelfLink;
        public ServiceUrlsDefinition serviceUrls;
        public String descriptionShort;
        public String descriptionLong;

    }

    /**
     * Represents the response for csp service definition API.
     */
    @Data
    public static class ServiceDefinition {
        String name;
        String displayName;
        String descriptionShort;
        @JsonProperty("healthCheckURL") // Make csp and CheckStyle happy!
                String healthCheckUrl;
    }

    /**
     * Represents the body to register the service as an oauth2 client.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class OauthClient {
        String id;
        String secret;
        String redirectUri;
        String[] redirectUris;
        String[] grantTypes;
        @JsonProperty("accessTokenTTL")
        int accessTokenTtl;
        @JsonProperty("refreshTokenTTL")
        int refreshTokenTtl;
        String resourceLink;
    }

    /**
     * Represents scopes to add or remove from Oauth Clients.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class OauthClientScopes {
        String[] scopesToAdd;
        String[] scopesToRemove;
        String serviceDefinitionLink;
    }
    /**
     * Represents the request for update service definition API.
     */

    // Do Not send "null" values. For BC for the field isGated & productIdentifier, I am not sure if its avbl pre-gaz.
    @Data
    @JsonInclude(Include.NON_EMPTY)
    public static class ServiceDefinitionUpdateRequest {
        @JsonProperty("isGated")
        private Boolean isGated; // FOR BC. This would be set to null instead of "false" and won't be sent
        private String productIdentifier;
        private String descriptionLong;
        private SubscriptionLifecycleEventHandlers subscriptionLifecycleEventHandlers;
        private ServiceUrlsDefinition serviceUrls;
        private String subscriptionStatusChangeNotificationUrl;
        private String serviceIcon;
        private String serviceNavBarIcon;
    }

    /**
     * Represents the body for subscription lifecycle callback handlers.
     */
    @Data
    public static class SubscriptionLifecycleEventHandlers {
        String onFulfillment;
        String onExpiration;
        String onCancellation;
    }

    /**
     * Represents the response from CSP on cancel subscription.
     */
    @Data
    public static class CspCancelSubscriptionResponse {

        String refLink;
    }

    /**
     * Represents the base callback from CSP.
     */
    @Data
    public static class CspSubscriptionCallback {

        private String userRefLink;
        private String orgRefLink;
        private String subscriptionRefLink;

    }

    /**
     * Enum to hold Csp Task Statuses.
     */
    public enum CspTaskStatus {

        COMPLETE("FINISHED"),
        FAILED("FAILED");

        private String statusText;

        CspTaskStatus(String statusText) {
            this.statusText = statusText;
        }

        /**
         * Get text of the status.
         *
         * @return String representation of the status.
         */
        public String getStatusText() {
            return statusText;
        }
    }

    /**
     * CSP Service URL definition.
     */
    @Data
    @NoArgsConstructor
    public static class ServiceUrlsDefinition {

        private String termsOfService;
        private String offerConfiguration;
        private String serviceHome;
        private String requestAccess;
    }

    /**
     * Holds the register client response.
     */
    @Data
    public static class CspRegisterClientResponse {

        private String id;
        private String redirectUri;
        private List<String> redirectUris;
        private String refLink;
        private List<String> grantTypes;
        @JsonProperty("accessTokenTTL") // Make csp and CheckStyle happy!
        private Integer accessTokenTtl;
        @JsonProperty("refreshTokenTTL") // Make csp and CheckStyle happy!
        private Integer refreshTokenTtl;
        private List<String> allowedScopes;
    }

    /**
     * Hold csp auth response.
     */
    @Data
    @NoArgsConstructor
    public static class CspLoginResponse {
        private String cspAuthToken;
    }

    /**
     * Service role definition.
     */
    @Data
    @AllArgsConstructor
    public static class CspServiceRole {
        private String name;
        private String displayName;
        @JsonProperty("isDefault")
        private boolean isDefault;
        private String status; // ACTIVE, IN_DELETION_PROCESS
        @JsonProperty("isHidden")
        private boolean isHidden; // should this show in usermgmt UI
    }

    /**
     * Service role status.
     */
    public enum CspServiceRoleStatus {
        ACTIVE,
        IN_DELETION_PROCESS
    }

    /**
     * Individual entry.
     */
    @Data
    public static class CspServiceRoleEntry {
        String serviceDefinitionLink;
        List<String> serviceRoleNames;
    }

    /**
     * Response from loggedin/user/orgs/{orgId}/service-roles.
     */
    @Data
    public static class CspUserServiceRolesResponse {
        List<CspServiceRoleEntry> serviceRoles;
    }

    /**
     * Holds the response from the csp authorize call.
     */
    @Data
    public static class CspAuthorizeResponse {
        @JsonProperty("access_token")
        String cspAuthToken;
        @JsonProperty("refresh_token")
        String cspRefreshToken;
        @JsonProperty("id_token")
        String idToken;
        @JsonProperty("token_type")
        String tokenType;
        @JsonProperty("expires_in")
        int expiresIn;
        String scope;
    }

    /**
     * Holds the csp request to logout.
     */
    @Data
    @AllArgsConstructor
    public static class CspLogoutRequst {
        String idToken;
    }

    /**
     * Holds the csp response to logout.
     */
    @Data
    public static class CspLogoutResponse {
        String url;
    }

    /**
     * A common class to hold ref links that are returned.
     */
    @Data
    public static class CspRefLinksResponse {

        private List<String> refLinks;
    }

    /**
     * A request to csp to generate invitations.
     * <ul>
     *  <li>
     *      {@link CspServiceInvitationRequest#serviceDefinitionLink} - The link to the service definition to which
     *      invitation is required.
     *  </li>
     *  <li>
     *      {@link CspServiceInvitationRequest#numberOfInvitationsToGenerate} - How many invites to be generated.
     *  </li>
     *  <li>
     *      {@link CspServiceInvitationRequest#context} - A map of properties that will be
     *      returned with the invitation.
     *  </li>
     * </ul>
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(Include.NON_EMPTY)
    public static class CspServiceInvitationRequest {

        private String serviceDefinitionLink;
        private Integer numberOfInvitationsToGenerate;
        private HashMap<String, String> context;
        // We need the class because we shouldn't send this property unless we want to set it to true.
        @JsonProperty("isFundMandatory")
        private Boolean isFundMandatory;
        private String orderId;
    }

    /**
     * A csp service invitation.
     */
    @Data
    @NoArgsConstructor
    public static class CspServiceInvitation {

        private String invitationLink;
        private String serviceDefinitionLink;
        private Long expirationTime;
        private String status;
        private String tosSignedBy;
        private String orgLink;
        private HashMap<String, String> context;
        @JsonProperty("isFundMandatory")
        private Boolean isFundMandatory;
        private String orderId;
    }

    /**
     * Hold response to CSP auth/token-public-key call.
     */
    @Data
    public static class CspTokenPublicKeyResponse {
        String alg;
        String value;
        String issuer;
    }

    /**
     * Hold response to CSP auth/token-public-key call.
     */
    @Data
    public static class CspJwksResponse {

        String issuer;
        List<Jwk> keys;

        /**
         * Class representing a Json Web Key (JWK).
         */
        @Data
        public static class Jwk {

            String kty;
            String kid;
            @JsonProperty("e")
            String exponent;
            @JsonProperty("n")
            String modulus;
        }
    }


    /**
     * Paginated response container.
     * @param <T> - The type of result in the response.
     */
    @Data
    @NoArgsConstructor
    public static class PaginatedResponse<T> {

        private List<T> results = new ArrayList<T>();
        private Integer totalResults;

    }

    /**
     * Billing api objects.
     **/
    @Deprecated
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class Offer {

        private String name;
        private String version;
        private List<ServiceDetail> serviceDetails;
        private String startDate;
        private String endDate;
        private String minimumQuantity;
        private List<String> currencies;
        private List<Service> services;
        private List<String> subTenants;

    }

    /**
     * Billing api objects.
     **/
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class OfferExpandedDto {

        private String name;
        private String version;
        private String startDate;
        private String endDate;
        private Integer minimumQuantity;
        private List<String> currencies;
        private List<OfferServiceDetail> serviceDetails;
        private List<Service> services;
        private List<String> subTenants;
    }

    /**
     * Service detail.
     */
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class OfferServiceDetail {

        private String customerSegment;
        private String geo;
        private String regionCode;
    }

    /**
     * Service detail.
     */
    @Deprecated
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class ServiceDetail {

        private String geo;

        @JsonProperty("region")
        private String sdpRegionCode;

        private String customerSegment;
    }

    /**
     * A service that is being subscribed to.
     */
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class Service {

        private String serviceType;
        private String serviceTypeDescription;
        private List<CommitmentTerm> commitmentTerms;

    }

    /**
     * Commitment term for the subscription.
     */
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class CommitmentTerm {

        private String commitmentTerm;
        @JsonProperty("commitmentTermUOM")
        private String commitmentTermUom;
        private List<String> paymentType;

    }

    /**
     * Create an offer subscription for a user in SDP.
     */
    @Data
    @NoArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class CreateOfferSubscriptionRequest {

        private String billablePersona;
        private String offerName;
        private String offerVersion;
        private String orgLink;
        private String serviceDefinitionId;
        private String customerSegment;
        private String commitmentTerm;

        @JsonProperty("regionCode")
        private String sdpRegionCode;
        private Integer quantity;
        @JsonProperty("dataCenter")
        private String geo;
        private String serviceType;
        private String paymentType;
        private Map<String, String> context;
    }

    /**
     * Create an offer subscription for a user in SDP.
     */
    @Data
    @NoArgsConstructor
    public static class CreateOfferSubscriptionResponse {
        private String orgLink;
        private String serviceDefinitionId;
        private String sid;
        private String refLink;
    }

    /**
     * Create an offer subscription for a user in SDP.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SubscriptionActivationRequest {
        @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DateTimeUtils.ISO_8601_DATE_TIME_PATTERN)
        private Date fulfillmentStartTime;
        @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DateTimeUtils.ISO_8601_DATE_TIME_PATTERN)
        private Date fulfillmentEndTime;
    }

    /**
     * An offer subscription in CSP.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(Include.NON_NULL)
    public static class CspOfferSubscription {

        private String refLink;
        private String sid;
        private String orgLink;
        private String serviceDefinitionId;
        private String eaNumber;
        private String startDate;
        private String endDate;
        private String status;
        private Map<String, String> context;
        private String autoRenewedAllowed;
        private String anniversaryBillingDate;
        private List<SubscriptionComponent> subscriptionComponents;
    }

    /**
     * Components of an offer subscription. This will be the link to the offer.
     */
    @Data
    @NoArgsConstructor
    public static class SubscriptionComponent {

        private String offerName;
        private String offerVersion;
        private String quantity;
        private String description;
        private String geo;
        private String commitmentTerm;
        // commitment term unit of measure
        @JsonProperty("commitmentTermUOM")
        private String commitmentTermUnit;
        @JsonProperty("regionCode")
        private String sdpRegionCode;
    }

    /**
     * Patch body for creating org role.
     */
    @Data
    @NoArgsConstructor
    public static class CspPatchServiceRolesRequest {
        private String serviceDefinitionLink;
        private List<String> roleNamesToAdd;
    }

    /**
     * Arguments for subscribing to subscription offer status changes.
     */
    @Data
    @NoArgsConstructor
    public static class SubscriptionStatusUpdateNotificationsRequest {
        private String messageType;
        private String endpoint;
        private String clientId;
        private String attributes;
    }

    /**
     * Response from a SubscriptionStatusUpdateRequest.
     */
    @Data
    @NoArgsConstructor
    public static class SubscriptionStatusUpdateNotificationsResponse {
        private String subscriptionId;
    }

    /**
     * model for the subscription status update callback.
     */
    @Data
    @NoArgsConstructor
    public static class CspSubscriptionStatusUpdate {
        String serviceDefinitionId;
        String subscriptionId;
        String organizationId;
        String currentStatus;
        String previousStatus;
        // TODO enable these properties
        //    private  eventTime;
        //    private  changeReasons;
    }

    /**
     * model for the subscription offer callback.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CspNotificationMessage {
        private UUID messageId;
        private UUID subscriptionId;
        private String messageTime;
        private String messageType;
        private UUID publisherId;
        private String message;
        private String callbackUri;
    }

    /**
     * Represents a subscription from CSP.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Subscription {
        private String messageType;
        private String endpoint;
        private String clientId;
        private String attributes;
    }

    /**
     * Represents an array of subscriptions from CSP.
     */
    @Data
    @NoArgsConstructor
    public static class Subscriptions {
        private Subscription[] subscriptions;
    }
}
